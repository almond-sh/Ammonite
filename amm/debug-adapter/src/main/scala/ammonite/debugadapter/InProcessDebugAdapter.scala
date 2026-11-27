package ammonite.debugadapter

import ch.epfl.scala.debugadapter.{
  AmmoniteDebugSession,
  CancelableFuture,
  DebugConfig,
  Debuggee,
  DebuggeeListener,
  JavaRuntime,
  Library,
  Logger,
  Module,
  ScalaVersion,
  SourceDirectory,
  SourceJar,
  UnmanagedEntry
}
import com.sun.jdi.Bootstrap

import java.io.{Closeable, File}
import java.net.{InetAddress, InetSocketAddress, ServerSocket, URI}
import java.nio.file.{Path, Paths}
import java.util.Properties
import java.util.concurrent.{ConcurrentLinkedQueue, Executors}
import java.util.function.Predicate

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

/**
 * Runs a scala-debug-adapter DAP server in the JVM of an Ammonite session (the Ammonite REPL, an
 * Almond kernel, …), attached to the JDWP agent of that JVM
 *
 * Sessions load this class in a class loader of its own, so that the dependencies of
 * scala-debug-adapter don't clash with theirs, and talk to it via reflection, with Java types
 * only: see [[InProcessDebugAdapter.start]]. Sessions then connect to [[uri]] to send DAP requests.
 * The server accepts a single connection, and stops once its DAP session is over.
 *
 * The JDI objects of scala-debug-adapter are wrapped by [[SafeVirtualMachineManager]], so that it
 * never suspends its own threads.
 */
final class InProcessDebugAdapter private (
    serverSocket: ServerSocket,
    classUpdates: InProcessDebugAdapter.ClassUpdates,
    tastyLines: Option[TastyLines],
    logger: Logger,
    pool: java.util.concurrent.ExecutorService
) {

  def uri(): String =
    s"tcp://${serverSocket.getInetAddress.getHostAddress}:${serverSocket.getLocalPort}"

  /**
   * Tells the adapter about classes compiled since the session started, that a `redefineClasses`
   * DAP request then takes into account
   */
  def classesUpdated(classNames: Array[String]): Unit =
    classUpdates.update(classNames.toSeq)

  /**
   * Tells the adapter that the line numbers of the TASTy files of classes are those of their byte
   * code plus `shift` (see [[TastyLines]])
   */
  def lineShift(classNames: Array[String], shift: Int): Unit =
    for (lines <- tastyLines)
      lines.update(classNames.toSeq, shift)

  @volatile private var closeSession: () => Unit = () => ()

  def close(): Unit = {
    try closeSession()
    catch {
      case NonFatal(e) =>
        logger.warn(s"Error closing debug session: $e")
    }
    try serverSocket.close()
    catch { case _: java.io.IOException => }
    pool.shutdown()
  }

  private[debugadapter] def run(
      debuggee: Debuggee,
      resolver: CoursierResolver,
      config: DebugConfig,
      suspendable: Predicate[String]
  ): Unit = {
    // The pool gets shut down once the session is over, or when we're closed, while the futures
    // of the session may still have callbacks to run, that don't matter any more then
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(
      new java.util.concurrent.Executor {
        def execute(command: Runnable): Unit =
          try pool.execute(command)
          catch {
            case _: java.util.concurrent.RejectedExecutionException if pool.isShutdown =>
              logger.debug(s"Ignoring task submitted after shutdown: $command")
          }
      }
    )
    val vmManager = SafeVirtualMachineManager(
      Bootstrap.virtualMachineManager(),
      t => suspendable.test(t.name())
    )
    Future {
      val socket = serverSocket.accept()
      serverSocket.close()
      val (close0, done) =
        AmmoniteDebugSession.run(
          socket,
          debuggee,
          resolver,
          logger,
          config,
          vmManager,
          tastyLines
        )
      closeSession = close0
      done.onComplete { res =>
        logger.debug(s"Debug session over ($res)")
        pool.shutdown()
      }
    }.failed.foreach { e =>
      logger.error(s"Error starting debug session: $e")
      logger.trace(e)
    }
  }
}

object InProcessDebugAdapter {

  /**
   * Starts a DAP server
   *
   * @param config
   *   - `jdwp.host` and `jdwp.port`: where the JDWP agent of the session listens (only used by
   *     `launch` requests: sessions send `attach` requests with the address of the agent)
   *   - `scala.version`: the Scala version of the session
   *   - `java.home`: the JVM of the session
   *   - `class-path` and `source-path`: the class path and source JARs of the session (separated
   *     by the path separator)
   *   - `modules`: directories with the byte code of cells, along with their sources, as
   *     `classes-dir|source-dir` entries, separated by the path separator
   *   - `scalac-options`: the scalac options of the session, separated by new lines
   *   - `repositories`: where to fetch the expression compiler from, separated by `|`
   *   - `verbose`: `true` to log more
   * @param suspendable
   *   names of the threads that can be suspended when the adapter wants to suspend all threads
   * @param log
   *   receives the log messages of the adapter, prefixed by their level
   */
  def start(
      config: Properties,
      suspendable: Predicate[String],
      log: java.util.function.Consumer[String]
  ): InProcessDebugAdapter = {
    def prop(key: String): Option[String] =
      Option(config.getProperty(key)).map(_.trim).filter(_.nonEmpty)
    def paths(key: String): Seq[Path] =
      prop(key).toSeq.flatMap(_.split(File.pathSeparator)).filter(_.nonEmpty).map(Paths.get(_))

    val logger = new ConsumerLogger(prop("verbose").contains("true"), log)

    val address = new InetSocketAddress(
      prop("jdwp.host").getOrElse("127.0.0.1"),
      prop("jdwp.port").map(_.toInt).getOrElse(0)
    )
    val scalaVersion = ScalaVersion(prop("scala.version").getOrElse(sys.error("No scala.version")))
    val scalacOptions =
      prop("scalac-options").toSeq.flatMap(_.linesIterator).map(_.trim).filter(_.nonEmpty)
    val modules = prop("modules").toSeq
      .flatMap(_.split(File.pathSeparator))
      .filter(_.nonEmpty)
      .zipWithIndex
      .map {
        case (entry, idx) =>
          val (classes, sources) = entry.split('|') match {
            case Array(c, s) => (Paths.get(c), Some(Paths.get(s)))
            case Array(c) => (Paths.get(c), None)
            case _ => sys.error(s"Malformed module entry: $entry")
          }
          Module(
            s"session-$idx",
            Some(scalaVersion),
            scalacOptions,
            classes,
            sources.map(SourceDirectory(_)).toSeq
          )
      }
    val repositories =
      prop("repositories").toSeq.flatMap(_.split('|')).map(_.trim).filter(_.nonEmpty)

    val classUpdates = new ClassUpdates
    val debuggee = new SessionDebuggee(
      address,
      scalaVersion,
      prop("java.home").getOrElse(sys.props("java.home")),
      paths("class-path"),
      paths("source-path"),
      modules,
      classUpdates
    )

    val pool = Executors.newCachedThreadPool { r =>
      val t = new Thread(r, "ammonite-debug-adapter")
      t.setDaemon(true)
      t
    }
    val serverSocket = new ServerSocket(0, 1, InetAddress.getLoopbackAddress)
    // the Scala 2 decoder doesn't look at line numbers
    val tastyLines = if (scalaVersion.isScala3) Some(new TastyLines) else None
    val adapter =
      new InProcessDebugAdapter(serverSocket, classUpdates, tastyLines, logger, pool)
    adapter.run(
      debuggee,
      new CoursierResolver(repositories, logger),
      DebugConfig.default.copy(gracePeriod = 5.seconds),
      suspendable
    )
    adapter
  }

  private final class ConsumerLogger(
      verbose: Boolean,
      log: java.util.function.Consumer[String]
  ) extends Logger {
    def debug(msg: => String): Unit = if (verbose) log.accept(s"debug $msg")
    def info(msg: => String): Unit = if (verbose) log.accept(s"info $msg")
    def warn(msg: => String): Unit = log.accept(s"warn $msg")
    def error(msg: => String): Unit = log.accept(s"error $msg")
    def trace(t: => Throwable): Unit =
      if (verbose) {
        val sw = new java.io.StringWriter
        t.printStackTrace(new java.io.PrintWriter(sw))
        log.accept(s"debug $sw")
      }
  }

  /** Classes compiled since the session started, that hot code replacement should look at */
  final class ClassUpdates {
    private val listeners = new ConcurrentLinkedQueue[Seq[String] => Unit]
    def update(classes: Seq[String]): Unit =
      listeners.asScala.foreach(_(classes))
    def observe(f: Seq[String] => Unit): Closeable = {
      listeners.add(f)
      () => listeners.remove(f)
    }
  }

  /** The JVM of the session, that's already running: we only pass its JDWP address to the adapter */
  final class SessionDebuggee(
      address: InetSocketAddress,
      val scalaVersion: ScalaVersion,
      javaHome: String,
      classPath: Seq[Path],
      sourcePath: Seq[Path],
      val modules: Seq[Module],
      classUpdates: ClassUpdates
  ) extends Debuggee {
    def name = "ammonite-session"

    def run(listener: DebuggeeListener): CancelableFuture[Unit] = {
      listener.onListening(address)
      val promise = Promise[Unit]()
      new CancelableFuture[Unit] {
        def future: Future[Unit] = promise.future
        def cancel(): Unit = promise.trySuccess(())
      }
    }

    // Sources of the libraries, matched with their JARs by name, the way coursier names them
    // (lib-1.0.jar / lib-1.0-sources.jar)
    private lazy val sourceJars: Map[String, Path] =
      sourcePath
        .map(p => p.getFileName.toString.stripSuffix(".jar").stripSuffix("-sources") -> p)
        .toMap

    lazy val libraries: Seq[Library] =
      classPath.filter(_.getFileName.toString.endsWith(".jar")).map { jar =>
        val baseName = jar.getFileName.toString.stripSuffix(".jar")
        val (artifactId, version) = Option(jar.getParent)
          .flatMap(p => Option(p.getFileName))
          .map(_.toString)
          .filter(v => baseName.endsWith("-" + v))
          .map(v => (baseName.stripSuffix("-" + v), v))
          .getOrElse((baseName, ""))
        Library(artifactId, version, jar, sourceJars.get(baseName).map(SourceJar(_)).toSeq)
      }
    lazy val unmanagedEntries: Seq[UnmanagedEntry] =
      classPath.filter(!_.getFileName.toString.endsWith(".jar")).map(UnmanagedEntry(_))

    lazy val javaRuntime: Option[JavaRuntime] = JavaRuntime(javaHome)

    def observeClassUpdates(onClassUpdate: Seq[String] => Unit): Closeable =
      classUpdates.observe(onClassUpdate)
  }
}
