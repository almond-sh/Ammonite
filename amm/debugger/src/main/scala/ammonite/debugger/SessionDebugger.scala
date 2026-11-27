package ammonite.debugger

import java.io.File
import java.net.Socket
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.concurrent.{ExecutorService, Executors, ThreadFactory}
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.DurationInt
import scala.util.Try

/**
 * Debugs the code of an Ammonite session (REPL, Almond kernel, …), from the JVM of the session
 * itself
 *
 * scala-debug-adapter runs in the JVM of the session, in a class loader of its own (see
 * [[DebugAdapter]]), attached via JDWP to the JVM it runs in, which runs with a JDWP agent (see
 * [[Jdwp]]). This class talks DAP to it, and takes care of what's specific to Ammonite:
 *
 * Ammonite compiles the code of each input (REPL command, notebook cell) as a wrapper class
 * (`cmd3`, …), whose byte code has the line numbers of the input code. Breakpoints are set on
 * inputs by "key" (the wrapper name of an input, the hash of its code, … - see
 * [[SessionDebugger.Input]]), and get set on the wrapper classes of inputs by name, via
 * `dap-fqcn:` URIs, right before a wrapper class is loaded and run (see [[beforeRun]]).
 *
 * Stack frames of wrapper classes are mapped back to inputs, and the frames of code generated
 * by Ammonite (printing results, accessors) are skipped, both in stack traces and when stepping.
 *
 * Events of the adapter are passed to the event handler (see [[setEventHandler]]) once
 * filtered.
 *
 * @param jdwpAddress makes the JDWP agent of the session listen if it doesn't yet, and returns
 *                    its address
 * @param pkgName package of the wrapper classes (like `ammonite.$sess`)
 * @param startAdapter starts the debug adapter, attached to the JDWP agent of the session
 * @param sourcePaths directories the debug adapter looks the sources of classes up in
 */
final class SessionDebugger(
    jdwpAddress: () => Either[String, (String, Int)],
    pkgName: Seq[String],
    startAdapter: (String => Boolean) => DebugAdapter,
    sourcePaths: () => Seq[String],
    log: Logger
) {

  import SessionDebugger._

  private val threadCount = new AtomicInteger
  private val pool: ExecutorService = Executors.newCachedThreadPool(
    new ThreadFactory {
      def newThread(r: Runnable): Thread = {
        val t = new Thread(r, s"ammonite-debugger-${threadCount.incrementAndGet()}")
        t.setDaemon(true)
        t
      }
    }
  )
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(pool)

  private val lock = new Object

  @volatile private var eventHandler: ujson.Obj => Unit = (_: ujson.Obj) => ()

  // All these are guarded by lock

  private var adapterOpt = Option.empty[(DebugAdapter, DapClient)]
  private var started = false

  /** Breakpoints of inputs, by input key: DAP `SourceBreakpoint`s, with lines in the inputs */
  private val breakpoints0 = new mutable.LinkedHashMap[String, Seq[ujson.Obj]]

  /** Inputs that were run, by key, and by wrapper name */
  private val inputsByKey = new mutable.HashMap[String, Vector[Input]]
  private val inputsByWrapper = new mutable.HashMap[String, Input]

  private val stoppedThreads0 = new mutable.LinkedHashSet[Long]

  private var lastJdwpAddress = Option.empty[(String, Int)]

  /** Line shifts of the classes of inputs, see [[DebugAdapter.lineShift]] */
  private val lineShifts = new mutable.ListBuffer[(Seq[String], Int)]

  /** Name of the thread running inputs, if we saw one run */
  @volatile private var executeThreadName = Option.empty[String]

  /** Receives the events of the debug adapter, once filtered */
  def setEventHandler(handler: ujson.Obj => Unit): Unit =
    eventHandler = handler

  private def sendEvent(event: ujson.Obj): Unit =
    eventHandler(event)

  def isStarted: Boolean = lock.synchronized(started)

  /** The breakpoints of inputs, by input key */
  def breakpoints: Seq[(String, Seq[ujson.Obj])] = lock.synchronized(breakpoints0.toVector)

  /** The threads we know are stopped */
  def stoppedThreads: Seq[Long] = lock.synchronized(stoppedThreads0.toVector)

  /** The input whose wrapper class has that name, if it was run */
  def input(wrapperName: String): Option[Input] =
    lock.synchronized(inputsByWrapper.get(wrapperName))

  /**
   * Called right before the wrapper class of an input is loaded and run, from the thread
   * running it
   *
   * @param classNames the classes the input was compiled to
   * @param lineShift difference between the line numbers of the TASTy files of the classes of
   *                  the input (if any) and those of their byte code
   */
  def beforeRun(input: Input, classNames: Seq[String], lineShift: Option[Int]): Unit = {
    executeThreadName = Some(Thread.currentThread().getName)
    val (adapterOpt0, bpsOpt) = lock.synchronized {
      inputsByKey(input.key) = inputsByKey.getOrElse(input.key, Vector.empty) :+ input
      inputsByWrapper(input.wrapperName) = input
      for (shift <- lineShift) {
        lineShifts += classNames -> shift
        for ((adapter, _) <- adapterOpt)
          adapter.lineShift(classNames, shift)
      }
      if (started) (adapterOpt, breakpoints0.get(input.key).filter(_.nonEmpty))
      else (None, None)
    }
    // Have the debug adapter take the byte code of the new classes into account (it reads it to
    // recognize synthetic methods, say): its redefineClasses request reloads its class path,
    // and also redefines the classes that are already loaded, which isn't the case of these
    for ((adapter, client) <- adapterOpt0 if classNames.nonEmpty) {
      val res = Try {
        adapter.classesUpdated(classNames)
        Await.result(client.request("redefineClasses"), 30.seconds)
      }
      for (e <- res.failed)
        log.warn(s"Error updating the classes of the debug adapter", e)
    }
    for (bps <- bpsOpt; (_, client) <- adapterOpt0) {
      log.debug(s"Setting breakpoints of ${input.key} in ${input.wrapperName}")
      // Waiting for the breakpoints to be set, so that they're there when the class gets loaded
      val res = Try(Await.result(setWrapperBreakpoints(client, input, bps), 30.seconds))
      for (e <- res.failed)
        log.warn(s"Error setting breakpoints in ${input.wrapperName}", e)
    }
  }

  private def fullName(wrapperName: String): String =
    (pkgName :+ wrapperName).mkString(".")

  private def setWrapperBreakpoints(
      client: DapClient,
      input: Input,
      bps: Seq[ujson.Obj]
  ): Future[ujson.Obj] = {
    val shifted = bps.map { bp =>
      val bp0 = ujson.Obj.from(bp.value)
      for (line <- bp.value.get("line"))
        bp0("line") = line.num.toInt + input.lineOffset
      bp0
    }
    client.request(
      "setBreakpoints",
      ujson.Obj(
        "source" -> ujson.Obj(
          "name" -> s"${input.wrapperName}.sc",
          "path" -> s"dap-fqcn:${fullName(input.wrapperName)}"
        ),
        "breakpoints" -> ujson.Arr(shifted: _*),
        "lines" -> ujson.Arr(shifted.flatMap(_.value.get("line")): _*)
      )
    )
  }

  private def client(): DapClient =
    lock.synchronized(adapterOpt) match {
      case Some((_, client)) if !client.isClosed => client
      case _ => throw new Exception("Debugger not started")
    }

  /** Sends a request as is to the debug adapter */
  def request(command: String, args: ujson.Obj = ujson.Obj()): Future[ujson.Obj] =
    Future(client()).flatMap(_.request(command, args))

  private def onEvent(event: ujson.Obj): Unit = {
    val eventName = event.value.get("event").map(_.str).getOrElse("")
    val body = event.value.get("body").collect { case o: ujson.Obj => o }
    def threadId = body.flatMap(_.value.get("threadId")).collect { case n: ujson.Num =>
      n.num.toLong
    }
    def allThreads(key: String) =
      body.flatMap(_.value.get(key)).exists {
        case ujson.Bool(b) => b
        case _ => false
      }
    eventName match {
      case "stopped" =>
        val reason = body.flatMap(_.value.get("reason")).collect { case ujson.Str(s) => s }
        (reason, threadId) match {
          case (Some("step"), Some(id)) =>
            onStepStopped(event, id).failed.foreach { e =>
              log.warn("Error filtering step", e)
              lock.synchronized(stoppedThreads0 += id)
              sendEvent(event)
            }
          case (Some("breakpoint"), Some(id)) =>
            onBreakpointStopped(event, id).failed.foreach { e =>
              log.warn("Error filtering breakpoint", e)
              lock.synchronized(stoppedThreads0 += id)
              sendEvent(event)
            }
          case _ =>
            lock.synchronized(stoppedThreads0 ++= threadId)
            sendEvent(event)
        }
      case "continued" =>
        lock.synchronized {
          if (allThreads("allThreadsContinued")) stoppedThreads0.clear()
          else stoppedThreads0 --= threadId
        }
        sendEvent(event)
      case "hotcodereplace" =>
      // sent upon our redefineClasses requests
      case "breakpoint" =>
      // these refer to the breakpoints we set in wrapper classes, clients know nothing about
      case "initialized" =>
      // we take care of the configuration of the adapter ourselves
      case "terminated" | "exited" =>
        log.debug(s"Debug adapter sent $eventName event")
      case _ =>
        sendEvent(event)
    }
  }

  /**
   * Starts the debug adapter if needed, and sends it an `initialize` request
   *
   * @param args arguments of the `initialize` request
   */
  def initialize(args: ujson.Obj): Future[ujson.Obj] =
    Future {
      lock.synchronized {
        if (adapterOpt.isEmpty) {
          // the adapter only ever suspends all threads via pause requests, which we restrict to
          // the thread running inputs anyway
          val adapter = startAdapter(name => executeThreadName.contains(name))
          for ((classNames, shift) <- lineShifts)
            adapter.lineShift(classNames, shift)
          val socket = new Socket(adapter.uri.getHost, adapter.uri.getPort)
          val client = new DapClient(
            socket,
            onEvent,
            () => onAdapterClosed(),
            log
          )
          adapterOpt = Some((adapter, client))
        }
        adapterOpt.get._2
      }
    }.flatMap { client =>
      client.request("initialize", args)
    }

  private def onAdapterClosed(): Unit = {
    val wasStarted = lock.synchronized {
      val wasStarted = started
      for ((adapter, _) <- adapterOpt)
        adapter.stop()
      adapterOpt = None
      started = false
      stoppedThreads0.clear()
      wasStarted
    }
    if (wasStarted)
      log.debug("Connection to the debug adapter closed")
  }

  /**
   * Attaches the debug adapter to the JVM of the session, once [[initialize]] was called, and
   * sets the breakpoints of the inputs that were already run
   */
  def attach(): Future[ujson.Obj] = {
    val client0 = client()
    // The debug adapter keeps its JDWP connection open across sessions (see
    // SafeVirtualMachineManager in the debug-adapter module), and the agent stops listening once
    // connected, so the address only matters the first time
    val (host, port) = lock.synchronized {
      lastJdwpAddress match {
        case Some(addr) => addr
        case None =>
          val addr = jdwpAddress().fold(err => throw new Exception(err), identity)
          lastJdwpAddress = Some(addr)
          addr
      }
    }
    val args = ujson.Obj(
      "hostName" -> host,
      "port" -> port,
      "timeout" -> 30000,
      "sourcePaths" -> ujson.Arr(sourcePaths().map(ujson.Str(_)): _*)
    )
    for {
      resp <- client0.request("attach", args)
      _ <-
        if (resp.value.get("success").exists(_.bool))
          client0.request("configurationDone").map { done =>
            if (!done.value.get("success").exists(_.bool))
              log.warn(s"configurationDone failed: ${ujson.write(done)}")
          }
        else
          Future.successful(())
      _ = lock.synchronized {
        started = resp.value.get("success").exists(_.bool)
      }
      // breakpoints set earlier, for inputs that were already run
      _ <- Future.sequence {
        lock.synchronized(breakpoints0.toVector).flatMap {
          case (key, bps) =>
            lock.synchronized(inputsByKey.getOrElse(key, Vector.empty))
              .map(setWrapperBreakpoints(client0, _, bps))
        }
      }
    } yield resp
  }

  /**
   * Sets the breakpoints of an input, replacing the former ones
   *
   * The breakpoints are set in the inputs with that key that were already run, if any, and in
   * the ones run later on.
   *
   * @param bps DAP `SourceBreakpoint`s, with lines in the input code
   * @return whether each breakpoint could be set in the latest input with that key, if any was
   *         run
   */
  def setBreakpoints(key: String, bps: Seq[ujson.Obj]): Future[Option[Seq[Boolean]]] = {
    val (inputs, clientOpt) = lock.synchronized {
      if (bps.isEmpty) breakpoints0 -= key
      else breakpoints0(key) = bps
      (
        inputsByKey.getOrElse(key, Vector.empty),
        if (started) adapterOpt.map(_._2) else None
      )
    }
    val setAll = clientOpt match {
      case Some(client) => Future.sequence(inputs.map(setWrapperBreakpoints(client, _, bps)))
      case None => Future.successful(Nil)
    }
    setAll.map { responses =>
      responses.lastOption.map { resp =>
        resp.value.get("body")
          .flatMap(_.obj.get("breakpoints"))
          .toSeq
          .flatMap(_.arr)
          .map(_.obj.get("verified").exists(_.bool))
      }
    }
  }

  /** Forgets the breakpoints of all inputs */
  def clearBreakpoints(): Future[Unit] = {
    val keys = lock.synchronized(breakpoints0.keys.toVector)
    Future.sequence(keys.map(setBreakpoints(_, Nil))).map(_ => ())
  }

  /**
   * Sends a `stackTrace` request, and maps the frames of the wrapper classes of inputs to the
   * inputs they come from, dropping the frames of the session itself below the frames of the
   * inputs
   */
  def stackTrace(args: ujson.Obj): Future[ujson.Obj] =
    request("stackTrace", args).map(mapStackTrace)

  private def mapStackTrace(resp: ujson.Obj): ujson.Obj = {
    val body = resp.value.get("body").collect { case o: ujson.Obj => o }
    body match {
      case None => resp
      case Some(body0) =>
        val frames = body0.value.get("stackFrames").toSeq.flatMap(_.arr).collect {
          case o: ujson.Obj => o
        }
        val mapped = frames.map(frame => (frame, mapFrame(frame)))
        val lastInputFrameIdx = mapped.lastIndexWhere(_._2.nonEmpty)
        val kept =
          if (lastInputFrameIdx < 0) frames
          else
            mapped
              .take(lastInputFrameIdx + 1)
              .zipWithIndex
              // Frames the debug adapter deems generated, and that have no line number, like
              // those of the classes of lambdas (cmd3$Helper$$Lambda/0x….apply(int): void).
              // Some front-ends show generated frames in a subtle way, but not all of them tell
              // them apart. We keep the generated frames with line numbers (like those of the
              // specialized methods of the Scala 2 library, Range.foreach$mVc$sp, that have the
              // actual code of these methods), and the top frame, the one the thread is stopped
              // in.
              .filter {
                case ((f, _), idx) =>
                  val subtle = f.value.get("presentationHint").contains(ujson.Str("subtle"))
                  val hasLine = f.value.get("line").exists {
                    case n: ujson.Num => n.num > 0
                    case _ => false
                  }
                  idx == 0 || !subtle || hasLine
              }
              .map { case ((f, m), _) => m.getOrElse(f) }
        val newBody = ujson.Obj.from(body0.value)
        newBody("stackFrames") = ujson.Arr(kept: _*)
        if (lastInputFrameIdx >= 0)
          newBody("totalFrames") = kept.length
        val newResp = ujson.Obj.from(resp.value)
        newResp("body") = newBody
        newResp
    }
  }

  /** The input whose wrapper class a frame (from the debug adapter) is in, if any */
  private def frameInput(frame: ujson.Obj): Option[Input] = {
    val source = frame.value.get("source").collect { case o: ujson.Obj => o }
    val name = source.flatMap(_.value.get("name")).collect { case ujson.Str(s) => s }
    val path = source.flatMap(_.value.get("path")).collect { case ujson.Str(s) => s }
    val wrapperNameOpt = name
      .orElse(path.map(p => p.substring(p.lastIndexOf(File.separatorChar) + 1)))
      .filter(_.endsWith(".sc"))
      .map(_.stripSuffix(".sc"))
    wrapperNameOpt.flatMap(name => lock.synchronized(inputsByWrapper.get(name)))
  }

  private def mapFrame(frame: ujson.Obj): Option[ujson.Obj] =
    for {
      input <- frameInput(frame)
      line <- frame.value.get("line").map(_.num.toInt)
      inputLine = line - input.lineOffset
      if inputLine >= 1 && inputLine <= input.userCodeLines
      if !inOuterWrapper(frame, input)
    } yield {
      val frame0 = ujson.Obj.from(frame.value)
      frame0("line") = inputLine
      for (endLine <- frame.value.get("endLine").collect { case n: ujson.Num => n.num.toInt })
        frame0("endLine") = endLine - input.lineOffset
      frame0("source") = ujson.Obj(
        "name" -> input.sourceName,
        "path" -> input.sourcePath
      )
      for (name <- frame.value.get("name").collect { case ujson.Str(s) => s })
        frame0("name") = inputFrameName(name, input.wrapperName)
      frame0
    }

  /**
   * Whether a frame is in the outer wrapper classes of an input, rather than in its `Helper`
   * class, that has the code of the input
   *
   * With Scala 2, some code of these classes (like the static initializer of the wrapper
   * object, that creates the `Helper` instance) gets line numbers in the input code, which
   * aren't those of the code of the input.
   */
  private def inOuterWrapper(frame: ujson.Obj, input: Input): Boolean =
    input.hasHelper && {
      val name = frameName(frame)
      val w = input.wrapperName
      (name.startsWith(s"$w.") || name.startsWith(s"$w$$.")) && !name.startsWith(s"$w.Helper.")
    }

  /**
   * Whether a frame is in code Ammonite generates in the wrapper class of an input: accessors
   * of the vals of the input (whose line numbers aren't those of the input), or the code
   * printing the results of the input (`$main`, whose line numbers are those of the statements
   * whose results it prints)
   */
  private def isGenerated(frame: ujson.Obj): Boolean =
    frameInput(frame).nonEmpty && {
      val name = frame.value.get("name").collect { case ujson.Str(s) => s }.getOrElse("")
      mapFrame(frame).isEmpty || name.contains("$main")
    }

  private def frameName(frame: ujson.Obj): String =
    frame.value.get("name").collect { case ujson.Str(s) => s }.getOrElse("")

  /**
   * Whether the top frame of a stack is the code printing the results of an input, or an
   * accessor of a val of an input it calls (accessors have the line number of their val, in
   * Scala 2)
   */
  private def isPrintingResults(frames: Seq[ujson.Obj]): Boolean =
    frames match {
      case Seq(top, _*) if isGenerated(top) => true
      case Seq(top, caller, _*) =>
        val sameInput = frameInput(top).nonEmpty && frameInput(top) == frameInput(caller)
        // like "cmd3$Helper.b(): int"
        sameInput && frameName(caller).contains("$main") && frameName(top).contains("(): ")
      case _ => false
    }

  private def isInputCode(frame: ujson.Obj): Boolean =
    mapFrame(frame).nonEmpty && !isGenerated(frame)

  private def isSteppedOutOf(frame: ujson.Obj): Boolean =
    stepOutOf(frame)

  /**
   * Frames (from the debug adapter) to step out of when a thread stops there, like the frames
   * of a method like `breakpoint()`, that users call to stop at a given place
   */
  @volatile var stepOutOf: ujson.Obj => Boolean = (_: ujson.Obj) => false

  private def stackFrames(client: DapClient, threadId: Long): Future[Seq[ujson.Obj]] =
    client.request("stackTrace", ujson.Obj("threadId" -> threadId.toDouble)).map { resp =>
      resp.value.get("body")
        .flatMap(_.obj.get("stackFrames"))
        .toSeq
        .flatMap(_.arr)
        .collect { case o: ujson.Obj => o }
    }

  /**
   * Called when a breakpoint is hit: breakpoints of inputs are also hit by the code printing
   * the results of inputs, as it has the line numbers of the statements whose results it
   * prints. We resume the thread then.
   */
  private def onBreakpointStopped(event: ujson.Obj, threadId: Long): Future[Unit] = {
    val client0 = client()
    stackFrames(client0, threadId).flatMap { frames =>
      if (isPrintingResults(frames))
        client0.request("continue", ujson.Obj("threadId" -> threadId.toDouble)).map(_ => ())
      else if (frames.headOption.exists(isSteppedOutOf)) {
        // reported as is once we're out of the frames to step out of, with a hint
        val event0 = ujson.Obj.from(event.value)
        val body = event.value.get("body").collect { case o: ujson.Obj => o }
          .map(o => ujson.Obj.from(o.value))
          .getOrElse(ujson.Obj())
        body("steppedOut") = true
        event0("body") = body
        lock.synchronized {
          lastStep(threadId) = "stepOut"
          stepAfter -= threadId
          autoSteps(threadId) = 1
          stepOutEvent(threadId) = event0
        }
        client0.request("stepOut", ujson.Obj("threadId" -> threadId.toDouble)).map(_ => ())
      } else
        Future.successful {
          lock.synchronized(stoppedThreads0 += threadId)
          sendEvent(event)
        }
    }
  }

  // Step filtering, see onStepStopped

  /** Last step request sent for each thread */
  private val lastStep = new mutable.HashMap[Long, String]

  /** Step request to send once the thread stops again, while stepping out of generated code */
  private val stepAfter = new mutable.HashMap[Long, String]

  /** Number of steps we sent ourselves since the last step requested by the client */
  private val autoSteps = new mutable.HashMap[Long, Int]

  /**
   * Breakpoint events whose thread we're stepping out of [[stepOutOf]] frames, to be reported
   * (rather than a step event) once it's done, with `steppedOut: true` in their body
   */
  private val stepOutEvent = new mutable.HashMap[Long, ujson.Obj]

  private def maxAutoSteps = 20

  /** Sends a step request (`next`, `stepIn`, `stepOut`) */
  def step(command: String, args: ujson.Obj): Future[ujson.Obj] = {
    val threadId = args.value.get("threadId").collect { case n: ujson.Num => n.num.toLong }
    lock.synchronized {
      for (id <- threadId) {
        lastStep(id) = command
        stepAfter -= id
        autoSteps -= id
        stepOutEvent -= id
        stoppedThreads0 -= id
      }
    }
    request(command, args)
  }

  /**
   * Called when a step ends, decides whether to report it, or to step again
   *
   * Like the step filters of IDEs, this skips the code Ammonite generates in the wrapper
   * classes of inputs (accessors of the vals of inputs, …): when a step ends there, we step out
   * of it (and step in again after a step in), or repeat the step. When a step leaves the code
   * of inputs for the code of the session (past the end of an input, say), we resume the thread
   * rather than stopping in the session. Frames of libraries called by inputs are kept.
   */
  private def onStepStopped(event: ujson.Obj, threadId: Long): Future[Unit] = {
    val client0 = client()
    def again(command: String): Future[Unit] = {
      lock.synchronized(autoSteps(threadId) = autoSteps.getOrElse(threadId, 0) + 1)
      client0.request(command, ujson.Obj("threadId" -> threadId.toDouble)).map(_ => ())
    }
    def report(): Unit = {
      val event0 = lock.synchronized {
        autoSteps -= threadId
        stoppedThreads0 += threadId
        stepOutEvent.remove(threadId).getOrElse(event)
      }
      sendEvent(event0)
    }
    val pending = lock.synchronized {
      if (autoSteps.getOrElse(threadId, 0) >= maxAutoSteps) None
      else stepAfter.remove(threadId)
    }
    pending match {
      case Some(command) => again(command)
      case None =>
        stackFrames(client0, threadId).flatMap {
          frames =>
            val tooMany = lock.synchronized(autoSteps.getOrElse(threadId, 0) >= maxAutoSteps)
            val last = lock.synchronized(lastStep.getOrElse(threadId, "next"))
            frames.headOption match {
              case _ if tooMany =>
                report()
                Future.successful(())
              case Some(top) if isSteppedOutOf(top) =>
                again("stepOut")
              case Some(top) if isGenerated(top) =>
                // generated code of a wrapper class
                if (last == "stepIn") {
                  lock.synchronized(stepAfter(threadId) = "stepIn")
                  again("stepOut")
                } else
                  again(last)
              case _ if !frames.exists(isInputCode) =>
                // not called from the code of an input any more
                log.debug(s"Step left the code of inputs, resuming thread $threadId")
                client0
                  .request("continue", ujson.Obj("threadId" -> threadId.toDouble))
                  .map { _ =>
                    lock.synchronized {
                      autoSteps -= threadId
                      stepOutEvent -= threadId
                      stoppedThreads0 -= threadId
                    }
                    sendEvent(continuedEvent(threadId))
                  }
              case _ =>
                report()
                Future.successful(())
            }
        }
    }
  }

  /**
   * Pausing all threads would pause the session itself, and leave it unable to process
   * requests, so we only ever pause the thread running inputs, unless asked for another one
   */
  def pause(threadIdOpt: Option[Long]): Future[ujson.Obj] = {
    val client0 = client()
    client0.request("threads").flatMap { threadsResp =>
      val threads = threadsResp.value.get("body")
        .flatMap(_.obj.get("threads"))
        .toSeq
        .flatMap(_.arr)
        .map(_.obj)
      val ids = threads.flatMap(_.get("id").map(_.num.toLong)).toSet
      val threadIdOpt0 = threadIdOpt.filter(ids).orElse {
        executeThreadName.flatMap { name =>
          threads.find(_.get("name").exists(_.str == s"Thread [$name]"))
            .flatMap(_.get("id").map(_.num.toLong))
        }
      }
      threadIdOpt0 match {
        case Some(id) =>
          client0.request("pause", ujson.Obj("threadId" -> id.toDouble))
        case None =>
          Future.failed(new Exception("No thread to pause"))
      }
    }
  }

  /**
   * Resumes threads
   *
   * The adapter doesn't send continued events for continue requests (DAP doesn't require it),
   * we send some to the event handler for each thread that was resumed.
   */
  def continueThreads(threadIds: Seq[Long]): Future[ujson.Obj] = {
    val client0 = client()
    lock.synchronized(stoppedThreads0 --= threadIds)
    val res = Future
      .sequence(
        threadIds.map(id => client0.request("continue", ujson.Obj("threadId" -> id.toDouble)))
      )
      .map { responses =>
        responses.lastOption match {
          case Some(last) =>
            val last0 = ujson.Obj.from(last.value)
            last0("body") = ujson.Obj("allThreadsContinued" -> (threadIds.length > 1))
            last0
          case None =>
            ujson.Obj("success" -> true, "body" -> ujson.Obj())
        }
      }
    res.map { resp =>
      if (resp.value.get("success").contains(ujson.True))
        for (id <- threadIds)
          sendEvent(continuedEvent(id))
      resp
    }
  }

  /** Disconnects the debug adapter, without terminating the session */
  def disconnect(args: ujson.Obj = ujson.Obj()): Future[ujson.Obj] = {
    val args0 = ujson.Obj.from(args.value)
    // Never terminate the session
    args0("terminateDebuggee") = false
    args0("restart") = false
    val res = request("disconnect", args0)
    res.andThen {
      case _ =>
        lock.synchronized {
          started = false
          breakpoints0.clear()
          stoppedThreads0.clear()
          for ((adapter, client) <- adapterOpt) {
            client.close()
            adapter.stop()
          }
          adapterOpt = None
        }
    }
  }

  /**
   * Stops the debug adapter
   *
   * The thread pool of this class isn't shut down: closing the connection to the adapter
   * completes futures whose callbacks still need to run there. Its threads are daemon ones,
   * that go away once idle.
   */
  def shutdown(): Unit =
    lock.synchronized {
      for ((adapter, client) <- adapterOpt) {
        client.close()
        adapter.stop()
      }
      adapterOpt = None
    }
}

object SessionDebugger {

  /**
   * An input (REPL command, notebook cell, …) that was run
   *
   * @param wrapperName name of the wrapper class of the input, like `cmd3`
   * @param key key of the breakpoints of the input, see [[SessionDebugger.setBreakpoints]]
   * @param sourceName name of the source of the input, in the stack frames of the input
   * @param sourcePath path of the source of the input, in the stack frames of the input
   * @param lineOffset offset of the line numbers of the input code in the byte code of its
   *                   wrapper class
   * @param userCodeLines number of lines of the code of the input
   * @param hasHelper whether the code of the input is in a `Helper` class, nested in the
   *                  wrapper class (class-based wrappers), rather than directly in the wrapper
   *                  object
   */
  final case class Input(
      wrapperName: String,
      key: String,
      sourceName: String,
      sourcePath: String,
      lineOffset: Int,
      userCodeLines: Int,
      hasHelper: Boolean
  )

  def continuedEvent(threadId: Long): ujson.Obj =
    ujson.Obj(
      "type" -> "event",
      "event" -> "continued",
      "body" -> ujson.Obj(
        "threadId" -> threadId.toDouble,
        "allThreadsContinued" -> false
      )
    )

  // values of objects whose class overrides toString, like `cmd2$Helper$P@9285 "P(1,2.5)"`
  private val objectWithToString = """(?s)[^"]*@\d+ "(.*)"""".r
  // values of arrays, like `String[2]@9283`
  private val arrayValue = """([^"@]*\[\d+\])@\d+""".r

  /**
   * Makes the value of a variable of the debug adapter more readable: the result of `toString`
   * for objects that override it (`P(1,2.5)` rather than `cmd2$Helper$P@9285 "P(1,2.5)"`), the
   * size of arrays (`String[2]` rather than `String[2]@9283`), and quoted characters
   */
  def displayValue(value: String, tpe: Option[String]): String =
    (tpe, value) match {
      case (Some("char"), _) => s"'$value'"
      case (_, objectWithToString(str)) => str
      case (_, arrayValue(array)) => array
      case _ => value
    }

  // like "$anonfun$r$1(cmd3$Helper,int): int" or "$init$$$anonfun$1(int): void"
  private val jvmAnonFun = """\$.*anonfun\$[^(]*\((.*)\)(.*)""".r

  /**
   * Name of a frame of the code of an input, without the names of the wrapper classes of the
   * input
   *
   * The debug adapter names frames like `cmd3.Helper.f(x: Int): Int` (methods of Scala 3 inputs
   * it decodes) or `cmd3$Helper.f(int): int` (the JVM names of methods, for Scala 2 inputs).
   * These become `f(x: Int): Int` and `f(int): int`. The constructor of the wrapper class, that
   * runs the code of the input, becomes `cmd3`, and the JVM names of lambdas become
   * `<anon fun>`, like Scala 3 names them.
   */
  def inputFrameName(name: String, wrapperName: String): String = {
    val helperClass = s"$wrapperName$$Helper"
    val prefixes = Seq(
      s"$wrapperName.Helper.",
      s"$helperClass.",
      s"$wrapperName.",
      s"$wrapperName$$."
    )
    prefixes.find(name.startsWith).map(p => name.stripPrefix(p)) match {
      case None => name
      case Some(rest) if rest.startsWith("<init>(") || rest.startsWith("<clinit>(") =>
        wrapperName
      case Some(jvmAnonFun(params, ret)) =>
        // dropping the wrapper instance, that Scala 2 lambdas get as first parameter
        val params0 = params.split(',').filter(p => p.nonEmpty && p != helperClass)
        s"<anon fun>(${params0.mkString(",")})$ret"
      case Some(rest) => rest
    }
  }

  /** The name of the JAR and the path of the entry of a `jar:` URI */
  def jarSource(uri: String): Option[(String, String)] =
    if (uri.startsWith("jar:")) {
      val idx = uri.indexOf("!/")
      if (idx < 0) None
      else {
        val jar = uri.substring("jar:".length, idx)
        val entry = uri.substring(idx + 2)
        // URI-decoding the JAR path (like %2B in com-microsoft-java-debug-core-0.34.0%2B44.jar)
        val jarPath = Try(new java.net.URI(jar).getPath).toOption.filter(_ != null).getOrElse(jar)
        val jarName = jarPath.substring(jarPath.lastIndexOf('/') + 1)
        if (jarName.isEmpty || entry.isEmpty) None
        else Some((jarName, entry))
      }
    } else None

  /** Reads a source, from a file, or from a JAR if it's a `jar:` URI */
  def readSource(path: String): Option[String] =
    if (path.startsWith("jar:"))
      Try {
        val conn = new java.net.URL(path).openConnection()
        conn.setUseCaches(false)
        val is = conn.getInputStream
        try {
          val baos = new java.io.ByteArrayOutputStream
          val buf = new Array[Byte](16384)
          var n = is.read(buf)
          while (n >= 0) {
            baos.write(buf, 0, n)
            n = is.read(buf)
          }
          new String(baos.toByteArray, StandardCharsets.UTF_8)
        } finally is.close()
      }.toOption
    else
      Try(Paths.get(path)).toOption.filter(Files.isRegularFile(_)).flatMap { p =>
        Try(new String(Files.readAllBytes(p), StandardCharsets.UTF_8)).toOption
      }

  /** Waits for a future, rethrowing its exception if it fails */
  def await[T](f: Future[T]): T =
    Await.result(f, scala.concurrent.duration.Duration.Inf)

  /** Whether a DAP response says its request succeeded */
  def succeeded(resp: ujson.Obj): Boolean =
    resp.value.get("success").contains(ujson.True)

  /** The error message of a DAP response, if it failed */
  def errorMessage(resp: ujson.Obj): Option[String] =
    if (succeeded(resp)) None
    else
      Some(
        resp.value.get("message").collect { case ujson.Str(s) => s }
          .orElse(
            resp.value.get("body").flatMap(_.obj.get("error")).flatMap(_.obj.get("format"))
              .collect { case ujson.Str(s) => s }
          )
          .getOrElse("Request failed")
      )
}
