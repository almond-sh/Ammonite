package ammonite.repl.bsp

import java.io.IOException
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import java.nio.file.{Path, Paths}
import java.util.concurrent.{ExecutorService, Executors, ThreadFactory}
import java.util.concurrent.atomic.AtomicInteger

import ammonite.runtime.{Frame, FrameOutputs}
import ch.epfl.scala.bsp4j.{
  BuildClient,
  BuildTargetEvent,
  BuildTargetEventKind,
  BuildTargetIdentifier,
  DidChangeBuildTarget
}
import org.eclipse.lsp4j.jsonrpc.Launcher

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.control.NonFatal

/**
 * A BSP server for a REPL session, listening on a Unix domain socket
 *
 * Accepts any number of clients, and notifies all of them when frames get created, removed,
 * or modified.
 *
 * Requires Java 16 or later. Use [[ReplBspServer.start]] to create one, and [[close]] to
 * stop it.
 *
 * @param socketPath the socket clients should connect to
 */
final class ReplBspServer private (
    val socketPath: Path,
    serverChannel: ServerSocketChannel,
    session: ReplBspSession,
    log: String => Unit
) extends AutoCloseable {
  import ReplBspServer._

  private val pool: ExecutorService = Executors.newCachedThreadPool(threadFactory("ammonite-bsp"))

  private final class Connection(channel: SocketChannel) {
    @volatile var client: Option[BuildClient] = None
    def close(): Unit =
      try channel.close()
      catch { case _: IOException => }
  }
  private val connections = mutable.Set.empty[Connection]
  @volatile private var closed = false

  // the frames clients were last told about, guarded by announcedLock
  private val announcedLock = new Object
  private var announced = session.frames.map(session.targetId).toSet

  private val listener: FrameOutputs.Listener =
    new FrameOutputs.Listener {
      def frameCreated(frame: Frame): Unit = notifyClients(None)
      def frameChanged(frame: Frame): Unit = notifyClients(Some(frame))
    }
  session.outputs.addListener(listener)

  private def notifyClients(changed: Option[Frame]): Unit = {
    val events = announcedLock.synchronized {
      val current = session.frames.map(session.targetId).toSet
      def event(id: BuildTargetIdentifier, kind: BuildTargetEventKind) = {
        val ev = new BuildTargetEvent(id)
        ev.setKind(kind)
        ev
      }
      val created = (current -- announced).toSeq.map(event(_, BuildTargetEventKind.CREATED))
      val deleted = (announced -- current).toSeq.map(event(_, BuildTargetEventKind.DELETED))
      val changed0 = changed
        .map(session.targetId)
        .filter(id => current(id) && announced(id))
        .toSeq
        .map(event(_, BuildTargetEventKind.CHANGED))
      announced = current
      created ++ deleted ++ changed0
    }
    if (events.nonEmpty) {
      val params = new DidChangeBuildTarget(events.asJava)
      for (conn <- connections.synchronized(connections.toVector); client <- conn.client)
        try client.onBuildTargetDidChange(params)
        catch {
          case NonFatal(e) =>
            log(s"Error notifying BSP client: $e")
        }
    }
  }

  private def serve(channel: SocketChannel): Unit = {
    val conn = new Connection(channel)
    connections.synchronized(connections += conn)
    try {
      val server = new ReplBuildServer(session, () => conn.close())
      val launcher = new Launcher.Builder[BuildClient]()
        .setExecutorService(pool)
        .setInput(UnixDomainSockets.inputStream(channel))
        .setOutput(UnixDomainSockets.outputStream(channel))
        .setRemoteInterface(classOf[BuildClient])
        .setLocalService(server)
        .create()
      conn.client = Some(launcher.getRemoteProxy)
      // returns when the connection gets closed, by the client or via build/exit
      launcher.startListening().get()
    } catch {
      case NonFatal(e) if !closed =>
        log(s"BSP connection error: $e")
    } finally {
      connections.synchronized(connections -= conn)
      conn.close()
    }
  }

  private val acceptThread: Thread = {
    val t = new Thread("ammonite-bsp-accept") {
      override def run(): Unit =
        try
          while (!closed) {
            val channel = serverChannel.accept()
            pool.submit(new Runnable { def run(): Unit = serve(channel) })
          }
        catch {
          case NonFatal(e) if !closed =>
            log(s"Error accepting BSP connections: $e")
          case NonFatal(_) => // closed
        }
    }
    t.setDaemon(true)
    t
  }

  /** Stops listening, closes all connections, and removes the socket file */
  def close(): Unit =
    if (!closed) {
      closed = true
      session.outputs.removeListener(listener)
      try serverChannel.close()
      catch { case _: IOException => }
      connections.synchronized(connections.toVector).foreach(_.close())
      pool.shutdown()
      os.remove(os.Path(socketPath, os.pwd), checkExists = false)
    }
}

object ReplBspServer {

  /**
   * Starts a BSP server for `session`, listening on a Unix domain socket at `socketPath`
   *
   * Relative paths are relative to the current directory. They allow to work around the
   * maximum length of socket paths (about 100 characters).
   *
   * Fails if another process listens on `socketPath` already. A socket file left there by a
   * process that isn't listening any more gets removed.
   */
  def start(
      socketPath: String,
      session: ReplBspSession,
      log: String => Unit = System.err.println(_)
  ): ReplBspServer = {
    val socketPath0 = Paths.get(socketPath)
    val path = os.Path(socketPath0, os.pwd)
    if (os.exists(path, followLinks = false)) {
      val inUse =
        try {
          UnixDomainSockets.connect(socketPath0).close()
          true
        } catch {
          case _: IOException => false
        }
      if (inUse)
        throw new IOException(s"Another process listens on BSP socket $path already")
      os.remove(path)
    }
    os.makeDir.all(path / os.up)
    val server =
      new ReplBspServer(socketPath0, UnixDomainSockets.listen(socketPath0), session, log)
    server.acceptThread.start()
    server
  }

  private def threadFactory(name: String): ThreadFactory =
    new ThreadFactory {
      val count = new AtomicInteger
      def newThread(r: Runnable): Thread = {
        val t = new Thread(r, s"$name-${count.incrementAndGet()}")
        t.setDaemon(true)
        t
      }
    }
}
