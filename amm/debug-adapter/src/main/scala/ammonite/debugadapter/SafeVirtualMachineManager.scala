package ammonite.debugadapter

import com.sun.jdi.{
  ObjectCollectedException,
  ThreadReference,
  VMDisconnectedException,
  VirtualMachine,
  VirtualMachineManager
}
import com.sun.jdi.connect.{AttachingConnector, Connector}
import com.sun.jdi.event.{Event, EventIterator, EventQueue, EventSet}
import com.sun.jdi.request.{EventRequest, EventRequestManager}

import java.lang.reflect.{InvocationHandler, InvocationTargetException, Method, Proxy}
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

/**
 * Wraps the JDI objects of the debug adapter, so that it can debug the JVM it runs in
 *
 * Whenever a JDI event request suspends all the threads of the debugged JVM, the threads of the
 * debug adapter itself get suspended too, and the JVM is frozen for good. So
 *   - class prepare requests (that java-debug creates with the default suspend policy, that
 *     suspends all threads) only suspend the thread loading the class (enough for breakpoints to
 *     be set before the class code runs),
 *   - suspending the whole VM only suspends the threads accepted by `suspendable` (the thread
 *     running cells, typically),
 *   - exiting the VM only disconnects from it (exiting would kill the session).
 *
 * Also, the JDWP connection is kept open once established, and shared by the successive debug
 * sessions: once a debugger is connected, the JDWP agent stops listening, and it starts listening
 * again when the debugger disconnects. Keeping the connection open ensures nothing else can
 * connect to the session. So disposing the VM only ends the current session: its event requests are
 * deleted, the threads it suspended are resumed, and its event queue reports the VM as
 * disconnected. The next attach starts a new session on the same connection.
 */
object SafeVirtualMachineManager {

  /** The JDWP connection, shared by the successive sessions */
  private final class Shared(val vm: VirtualMachine) {

    @volatile var alive = true
    @volatile private var current = Option.empty[Session]

    def newSession(): Session = synchronized {
      for (s <- current)
        s.end()
      val s = new Session(this)
      current = Some(s)
      s
    }

    def sessionEnded(s: Session): Unit = synchronized {
      if (current.contains(s))
        current = None
    }

    // The only reader of the actual event queue, dispatching events to the current session
    private val reader = new Thread("ammonite-jdi-events") {
      setDaemon(true)
      override def run(): Unit =
        try
          while (alive) {
            val set = vm.eventQueue().remove()
            current match {
              case Some(s) => s.queue.add(Right(SafeEventSet(set)))
              case None =>
                // nobody to handle that, the requests of the former session are gone anyway
                try set.resume()
                catch { case NonFatal(_) => }
            }
          }
        catch {
          case _: VMDisconnectedException | _: InterruptedException =>
            alive = false
            for (s <- current)
              s.queue.add(Left(()))
        }
    }
    reader.start()
  }

  /** A debug session, using the shared connection */
  private final class Session(shared: Shared) {
    // Left: the session is over (the VM looks disconnected to it)
    val queue = new LinkedBlockingQueue[Either[Unit, EventSet]]
    @volatile var ended = false

    def end(): Unit =
      if (!ended) {
        ended = true
        shared.sessionEnded(this)
        queue.add(Left(()))
      }

    /** Removes the event requests of this session, and resumes the threads it suspended */
    def cleanUp(): Unit = {
      val vm = shared.vm
      val erm = vm.eventRequestManager()
      val requests: Seq[EventRequest] =
        (erm.breakpointRequests().asScala ++
          erm.classPrepareRequests().asScala ++
          erm.classUnloadRequests().asScala ++
          erm.exceptionRequests().asScala ++
          erm.methodEntryRequests().asScala ++
          erm.methodExitRequests().asScala ++
          erm.monitorContendedEnterRequests().asScala ++
          erm.monitorContendedEnteredRequests().asScala ++
          erm.monitorWaitRequests().asScala ++
          erm.monitorWaitedRequests().asScala ++
          erm.stepRequests().asScala ++
          erm.threadStartRequests().asScala ++
          erm.threadDeathRequests().asScala ++
          erm.accessWatchpointRequests().asScala ++
          erm.modificationWatchpointRequests().asScala).toSeq
      try erm.deleteEventRequests(requests.asJava)
      catch { case NonFatal(_) => }
      // suspend counts only count the suspensions of debuggers
      for (t <- vm.allThreads().asScala)
        try
          while (t.suspendCount() > 0)
            t.resume()
        catch { case NonFatal(_) => }
    }
  }

  @volatile private var shared = Option.empty[Shared]

  def apply(
      underlying: VirtualMachineManager,
      suspendable: ThreadReference => Boolean
  ): VirtualMachineManager =
    proxy(classOf[VirtualMachineManager], underlying) {
      case ("attachingConnectors", _) =>
        underlying.attachingConnectors().asScala
          .find(_.name() == "com.sun.jdi.SocketAttach")
          .toList
          .map(connector(_, suspendable))
          .asJava
    }

  private def connector(
      underlying: AttachingConnector,
      suspendable: ThreadReference => Boolean
  ): AttachingConnector =
    proxy(classOf[AttachingConnector], underlying) {
      case ("attach", Array(args)) =>
        val shared0 = synchronized {
          shared.filter(_.alive).getOrElse {
            val vm = underlying.attach(args.asInstanceOf[java.util.Map[String, Connector.Argument]])
            val s = new Shared(vm)
            shared = Some(s)
            s
          }
        }
        virtualMachine(shared0, shared0.newSession(), suspendable)
    }

  private def virtualMachine(
      shared: Shared,
      session: Session,
      suspendable: ThreadReference => Boolean
  ): VirtualMachine = {
    val underlying = shared.vm
    lazy val erm = eventRequestManager(underlying.eventRequestManager())
    lazy val queue = eventQueue(underlying.eventQueue(), session)
    proxy(classOf[VirtualMachine], underlying) {
      case ("eventRequestManager", _) => erm
      case ("eventQueue", _) => queue
      case ("suspend", _) =>
        for (t <- underlying.allThreads().asScala if suspendable(t))
          t.suspend()
        null
      case ("dispose", _) | ("exit", _) =>
        if (!session.ended) {
          session.cleanUp()
          session.end()
        }
        null
    }
  }

  private def eventQueue(underlying: EventQueue, session: Session): EventQueue = {
    def next(timeoutMs: Option[Long]): EventSet = {
      val elem = timeoutMs match {
        case None => session.queue.take()
        case Some(ms) => session.queue.poll(ms, TimeUnit.MILLISECONDS)
      }
      elem match {
        case null => null
        case Left(()) =>
          // keep reporting the session as over
          session.queue.add(Left(()))
          throw new VMDisconnectedException
        case Right(set) => set
      }
    }
    proxy(classOf[EventQueue], underlying) {
      case ("remove", Array()) => next(None)
      case ("remove", Array(ms)) =>
        val ms0 = ms.asInstanceOf[java.lang.Long].longValue
        next(if (ms0 == 0L) None else Some(ms0))
    }
  }

  private def eventRequestManager(underlying: EventRequestManager): EventRequestManager =
    proxy(classOf[EventRequestManager], underlying) {
      case ("createClassPrepareRequest", _) =>
        val req = underlying.createClassPrepareRequest()
        req.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
        req
    }

  private def proxy[T](cls: Class[T], underlying: T)(
      intercept: PartialFunction[(String, Array[AnyRef]), Any]
  ): T = {
    val handler: InvocationHandler = new InvocationHandler {
      def invoke(self: Any, method: Method, args: Array[AnyRef]): AnyRef = {
        val args0 = if (args == null) Array.empty[AnyRef] else args
        method.getName match {
          case "equals" if args0.length == 1 =>
            Boolean.box(args0(0).asInstanceOf[AnyRef].eq(self.asInstanceOf[AnyRef]) ||
              args0(0) == underlying)
          case "hashCode" if args0.isEmpty => Int.box(underlying.hashCode())
          case "toString" if args0.isEmpty => underlying.toString
          case name =>
            intercept.lift((name, args0)) match {
              case Some(res) => res.asInstanceOf[AnyRef]
              case None =>
                try method.invoke(underlying, args0: _*)
                catch {
                  case e: InvocationTargetException => throw e.getCause
                }
            }
        }
      }
    }
    Proxy.newProxyInstance(cls.getClassLoader, Array[Class[_]](cls), handler).asInstanceOf[T]
  }

  /**
   * An event set, whose events can always be converted to strings
   *
   * The event hub of java-debug logs every event it gets (`"JDI Event: " + event`, even when that
   * log level is disabled). The `toString` method of the JDI events of threads reads the name of
   * their thread, and throws if that thread was garbage collected meanwhile (which happens with
   * the events of threads that just died, say). That kills the event hub thread, and the debug
   * session stops getting events. So we wrap events, catching these exceptions.
   */
  private final class SafeEventSet(underlying: EventSet, events: Seq[Event])
      extends java.util.AbstractSet[Event] with EventSet {
    def iterator(): java.util.Iterator[Event] = events.iterator.asJava
    def size(): Int = events.length
    def suspendPolicy(): Int = underlying.suspendPolicy()
    def eventIterator(): EventIterator = {
      val it = events.iterator
      new EventIterator {
        def hasNext: Boolean = it.hasNext
        def next(): Event = it.next()
        def nextEvent(): Event = it.next()
      }
    }
    def resume(): Unit = underlying.resume()
    def virtualMachine(): VirtualMachine = underlying.virtualMachine()
    override def toString: String = events.mkString("event set [", ", ", "]")
  }

  private object SafeEventSet {
    def apply(set: EventSet): EventSet =
      new SafeEventSet(set, set.asScala.toVector.map(safeEvent))
  }

  private def interfaces(cls: Class[_]): Seq[Class[_]] =
    if (cls == null) Nil
    else
      (cls.getInterfaces.toSeq.flatMap(i => i +: interfaces(i)) ++ interfaces(cls.getSuperclass))
        .distinct

  private def safeEvent(event: Event): Event = {
    val handler: InvocationHandler = new InvocationHandler {
      def invoke(self: Any, method: Method, args: Array[AnyRef]): AnyRef = {
        val args0 = if (args == null) Array.empty[AnyRef] else args
        method.getName match {
          case "toString" if args0.isEmpty =>
            try event.toString
            catch {
              case _: ObjectCollectedException | _: VMDisconnectedException =>
                event.getClass.getSimpleName
            }
          case "equals" if args0.length == 1 =>
            Boolean.box(args0(0).asInstanceOf[AnyRef].eq(self.asInstanceOf[AnyRef]) ||
              args0(0) == event)
          case "hashCode" if args0.isEmpty => Int.box(event.hashCode())
          case _ =>
            try method.invoke(event, args0: _*)
            catch {
              case e: InvocationTargetException => throw e.getCause
            }
        }
      }
    }
    Proxy
      .newProxyInstance(
        classOf[Event].getClassLoader,
        interfaces(event.getClass).filter(_.getName.startsWith("com.sun.jdi.")).toArray,
        handler
      )
      .asInstanceOf[Event]
  }
}
