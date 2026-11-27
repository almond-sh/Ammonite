package ammonite.repl.debugger

import java.io.{File, PrintStream}
import java.util.concurrent.LinkedBlockingQueue

import ammonite.debugger.{DebugAdapter, Jdwp, Logger, SessionDebugger}
import ammonite.repl.api.DebuggerAPI
import ammonite.runtime.{Frame, FrameOutputs}
import ammonite.compiler.iface.Parser
import ammonite.util.{Classpath, Colors, ImportTree, Ref}
import ammonite.util.Util.CodeSource

import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt
import scala.util.Try
import scala.util.control.NonFatal

/**
 * The debugger of the Ammonite REPL, with a gdb-like command-line interface
 *
 * Breakpoints are set with `:break <location>` at the REPL prompt (or via `repl.debugger`, see
 * [[DebuggerAPI]]), or by calling `breakpoint()` in the code of commands. When a thread stops
 * at one of them, the REPL shows a `(debug)` prompt, that accepts commands like `bt`,
 * `info locals`, `p <expr>`, `next`, `step`, `finish`, `continue` (see [[help]]).
 *
 * The prompt runs in a thread of its own, while the main thread, that runs the code of
 * commands, is suspended by the debugger.
 *
 * @param readLine reads a line from the terminal, with a prompt and a history
 * @param latestWrapperName name of the wrapper class of the latest command that was compiled
 *                          (like `cmd4`), if any
 */
final class ReplDebugger(
    frames: () => List[Frame],
    outputs: FrameOutputs,
    pkgName: Seq[String],
    scalaVersion: String,
    ammoniteVersion: String,
    repositories: () => Seq[coursierapi.Repository],
    initialClassLoader: ClassLoader,
    out: PrintStream,
    colors: Ref[Colors],
    readLine: (String, IndexedSeq[String]) => Option[String],
    latestWrapperName: () => Option[String],
    log0: Logger
) {

  import ReplDebugger._
  import SessionDebugger.{await, errorMessage, succeeded}

  /**
   * Our logger, but with the warnings and errors of the debug adapter as debug messages: it
   * reports as warnings things that don't matter much (like classes compiled without TASTy),
   * and its errors are those of the requests we send it, that we report ourselves
   */
  private val log: Logger = new Logger {
    private def isAdapter(msg: String) = msg.startsWith("[debug-adapter]")
    def error(msg: => String, t: Throwable): Unit = {
      val msg0 = msg
      if (isAdapter(msg0)) log0.debug(msg0, t) else log0.error(msg0, t)
    }
    def warn(msg: => String, t: Throwable): Unit = {
      val msg0 = msg
      if (isAdapter(msg0)) log0.debug(msg0, t) else log0.warn(msg0, t)
    }
    def info(msg: => String, t: Throwable): Unit = log0.info(msg, t)
    def debug(msg: => String, t: Throwable): Unit = log0.debug(msg, t)
  }

  private val core = new SessionDebugger(
    () => Jdwp.ensureListening(),
    pkgName,
    startAdapter,
    () => frames().reverse.map(outputs.sourcesDirectory(_).toString),
    log
  )
  import core.ec

  private def isScala3 = scalaVersion.startsWith("3.")

  private def startAdapter(suspendable: String => Boolean): DebugAdapter = {
    val classPath = DebugAdapter.classPath(ammoniteVersion, repositories())
    log.debug(s"Debug adapter class path: ${classPath.mkString(File.pathSeparator)}")
    val frames0 = frames()
    val sessionClassPath =
      (frames0.flatMap(_.classpath) ++ Classpath.classpath(initialClassLoader))
        .flatMap(u => Try(new File(u.toURI)).toOption)
        .filter(_.exists())
        .distinct
    // coursier puts the source JARs of libraries right next to their JARs
    val sourcePath = sessionClassPath
      .filter(_.getName.endsWith(".jar"))
      .map(f => new File(f.getParentFile, f.getName.stripSuffix(".jar") + "-sources.jar"))
      .filter(_.isFile)
    def pathString(files: Seq[File]) =
      files.map(_.getAbsolutePath).distinct.mkString(File.pathSeparator)
    val modules = frames0.reverse.map { frame =>
      s"${outputs.classesDirectory(frame)}|${outputs.sourcesDirectory(frame)}"
    }
    DebugAdapter.start(
      classPath,
      Map(
        "class-path" -> pathString(sessionClassPath),
        "source-path" -> pathString(sourcePath),
        "modules" -> modules.mkString(File.pathSeparator),
        "scala.version" -> scalaVersion,
        "java.home" -> sys.props("java.home"),
        "verbose" -> Option(System.getenv("AMMONITE_DEBUG_ADAPTER_VERBOSE")).getOrElse("false")
      ),
      suspendable,
      log
    )
  }

  // Breakpoints

  private val lock = new Object

  // guarded by lock
  private val breakpoints0 = new mutable.LinkedHashMap[Int, Location]
  private var nextBreakpointId = 1
  private var sourcesWithBreakpoints = Set.empty[String]

  /** Line of the breakpoint we set in [[DebuggerAPI.stopHere]] */
  private lazy val stopHereLine = DebuggerAPI.stopHere()

  private def stopHereClass = DebuggerAPI.getClass.getName

  /** Adds a breakpoint, and returns its number */
  def addBreakpoint(location: Location): Int = {
    val id = lock.synchronized {
      val id = nextBreakpointId
      nextBreakpointId += 1
      breakpoints0(id) = location
      id
    }
    syncBreakpoints(location)
    id
  }

  def breakpoints: Seq[(Int, Location)] = lock.synchronized(breakpoints0.toVector)

  /** Removes a breakpoint, returns whether it existed */
  def removeBreakpoint(id: Int): Boolean = {
    val removed = lock.synchronized(breakpoints0.remove(id))
    for (loc <- removed)
      syncBreakpoints(loc)
    removed.nonEmpty
  }

  def clearBreakpoints(): Unit = {
    val former = lock.synchronized {
      val former = breakpoints0.values.toVector
      breakpoints0.clear()
      former
    }
    for (loc <- former.groupBy(sourceKey).values.map(_.head))
      syncBreakpoints(loc)
  }

  private def sourceKey(loc: Location): String =
    loc match {
      case l: Location.Input => "input:" + l.wrapperName
      case l: Location.Class => "class:" + l.className
      case l: Location.File => "file:" + l.path
    }

  private def lines(locs: Seq[Location]): Seq[ujson.Obj] =
    locs.map(_.line).distinct.sorted.map(l => ujson.Obj("line" -> l))

  /** Sends the breakpoints of the source of `location` to the debug adapter */
  private def syncBreakpoints(location: Location): Unit = {
    val key = sourceKey(location)
    val locs = lock.synchronized(breakpoints0.values.filter(sourceKey(_) == key).toVector)
    location match {
      case l: Location.Input =>
        // kept by core until the input is run, if it isn't yet
        await(core.setBreakpoints(l.wrapperName, lines(locs)))
      case _ =>
        if (core.isStarted) await(setSourceBreakpoints(key, location, locs))
    }
  }

  private def sourceObj(loc: Location): ujson.Obj =
    loc match {
      case l: Location.Class => ujson.Obj("path" -> s"dap-fqcn:${l.className}")
      case l: Location.File =>
        ujson.Obj("path" -> l.path, "name" -> new File(l.path).getName)
      case _: Location.Input => sys.error("Unexpected input location")
    }

  private def setSourceBreakpoints(
      key: String,
      location: Location,
      locs: Seq[Location]
  ): Future[Unit] = {
    val lines0 = lines(locs) ++ {
      // the breakpoint of breakpoint(), in the same class
      if (key == sourceKey(stopHereLocation) && !locs.contains(stopHereLocation))
        lines(Seq(stopHereLocation))
      else Nil
    }
    lock.synchronized {
      sourcesWithBreakpoints =
        if (lines0.isEmpty) sourcesWithBreakpoints - key
        else sourcesWithBreakpoints + key
    }
    core.request(
      "setBreakpoints",
      ujson.Obj(
        "source" -> sourceObj(location),
        "breakpoints" -> ujson.Arr(lines0: _*),
        "lines" -> ujson.Arr(lines0.map(_("line")): _*)
      )
    ).map { resp =>
      for (msg <- errorMessage(resp))
        printError(s"Error setting breakpoints in ${location.sourceDescription}: $msg")
    }
  }

  private lazy val stopHereLocation: Location = Location.Class(stopHereClass, stopHereLine)

  /**
   * Parses a breakpoint location, like `cmd3.sc:4`, `cmd3:4`, `4`, `some.pkg.Foo:12`,
   * `/path/Foo.scala:12`
   *
   * @param defaultSource where breakpoints with no source (`4`) go
   */
  def parseLocation(
      spec: String,
      defaultSource: () => Either[String, Option[String] => Int => Location] = () =>
        latestWrapperName()
          .toRight("No command was run yet")
          .map(name => _ => line => Location.Input(name, line))
  ): Either[String, Location] = {
    val spec0 = spec.trim
    val idx = spec0.lastIndexOf(':')
    val (source, lineStr) =
      if (idx < 0) ("", spec0)
      else (spec0.substring(0, idx), spec0.substring(idx + 1))
    Try(lineStr.trim.toInt).toOption.filter(_ > 0) match {
      case None => Left(s"Invalid breakpoint location '$spec0' (expected [file:]line)")
      case Some(line) =>
        if (source.isEmpty) defaultSource().map(_(None)(line))
        else if (isInputName(source.stripSuffix(".sc")))
          Right(Location.Input(source.stripSuffix(".sc"), line))
        else if (
          source.contains("/") || source.contains(File.separator) ||
          source.endsWith(".scala") || source.endsWith(".java") || source.endsWith(".sc")
        )
          Right(Location.File(new File(source).getAbsolutePath, line))
        else
          Right(Location.Class(source, line))
    }
  }

  private def isInputName(name: String): Boolean =
    name.startsWith("cmd") && name.length > 3 && name.drop(3).forall(_.isDigit)

  // Starting the debugger

  private val initializeArgs = ujson.Obj(
    "clientID" -> "ammonite",
    "clientName" -> "Ammonite",
    "adapterID" -> "scala",
    "linesStartAt1" -> true,
    "columnsStartAt1" -> true,
    "pathFormat" -> "path",
    "supportsVariableType" -> true
  )

  private lazy val started: Future[Unit] =
    for {
      _ <- core.initialize(initializeArgs)
      resp <- core.attach()
      _ = for (msg <- errorMessage(resp)) throw new Exception(msg)
      // breakpoints in classes and source files, and the one of breakpoint()
      locs = lock.synchronized(breakpoints0.values.toVector)
        .filter(!_.isInstanceOf[Location.Input])
      _ <- Future.sequence {
        (locs :+ stopHereLocation).groupBy(sourceKey).toVector.map {
          case (key, locs0) =>
            setSourceBreakpoints(key, locs0.head, locs0.filter(_ != stopHereLocation))
        }
      }
    } yield ()

  /** Starts the debugger in the background, if it's not started yet */
  def startInBackground(): Unit = {
    started.failed.foreach { e =>
      printError(s"Error starting the debugger: $e")
      log.debug("Error starting the debugger", e)
    }
  }

  /** Starts the debugger if it's not started yet, and waits for it to be ready */
  def ensureStarted(): Unit =
    Await.result(started, 5.minutes)

  core.stepOutOf = { frame =>
    val sourceName = frame.value.get("source").collect { case o: ujson.Obj => o }
      .flatMap(_.value.get("name")).collect { case ujson.Str(s) => s }
    val name = frame.value.get("name").collect { case ujson.Str(s) => s }.getOrElse("")
    sourceName.contains("DebuggerAPI.scala") ||
    name.startsWith("DebuggerAPI$.") || name.startsWith("DebuggerAPI.")
  }

  DebuggerAPI.setBeforeBreakpoint { () =>
    try ensureStarted()
    catch {
      case NonFatal(e) =>
        printError(s"Debugger not available: $e")
    }
  }

  // Inputs

  /** Whether the main thread is running the code of a command */
  @volatile private var running = false

  /** Called right before the wrapper class of a command is loaded and run */
  def beforeRun(wrapperName: String, classNames: Seq[String]): Unit = {
    val frame = frames().head
    val source = outputs.sources(frame).reverseIterator.find(_.path.last == s"$wrapperName.sc")
    val code = source.flatMap(s => Try(os.read(s.path)).toOption).getOrElse("")
    val input = SessionDebugger.Input(
      wrapperName,
      wrapperName,
      s"$wrapperName.sc",
      source.map(_.path.toString).getOrElse(s"$wrapperName.sc"),
      0,
      code.count(_ == '\n') + 1,
      classNames.contains((pkgName :+ wrapperName).mkString(".") + "$Helper")
    )
    // Ammonite shifts the line numbers of the byte code of commands by the number of lines of
    // the code it wraps them in (the TASTy files keep the line numbers of the wrapped code)
    val lineShift = source.flatMap(_.wrapped).map(_.topWrapper.count(_ == '\n'))
    for (s <- source)
      sourceCache(s.path.toString) = code.linesIterator.toVector
    running = true
    // the breakpoints of the command are only set if the debugger is started
    if (lock.synchronized(breakpoints0.nonEmpty))
      try ensureStarted()
      catch {
        case NonFatal(e) =>
          printError(s"Debugger not available: $e")
      }
    core.beforeRun(input, classNames, lineShift)
  }

  /** Called once a command is done running */
  def afterRun(): Unit =
    running = false

  // Stops

  private val stops = new LinkedBlockingQueue[ujson.Obj]

  core.setEventHandler { event =>
    event.value.get("event").collect { case ujson.Str(s) => s } match {
      case Some("stopped") => stops.put(event)
      case _ =>
    }
  }

  private val consoleThread = new Thread("ammonite-debugger-console") {
    setDaemon(true)
    override def run(): Unit =
      while (true) {
        val event = stops.take()
        try onStop(event)
        catch {
          case NonFatal(e) =>
            printError(s"Debugger error: $e")
            log.debug("Debugger error", e)
            // don't leave the thread stopped
            for (id <- threadId(event))
              Try(await(core.continueThreads(Seq(id))))
        }
      }
  }
  consoleThread.start()

  private def threadId(event: ujson.Obj): Option[Long] =
    event.value.get("body").flatMap(_.obj.get("threadId")).collect { case n: ujson.Num =>
      n.num.toLong
    }

  private val history = mutable.ArrayBuffer.empty[String]

  private def onStop(event: ujson.Obj): Unit = {
    val body = event.value.get("body").collect { case o: ujson.Obj => o }
    val id = threadId(event).getOrElse(throw new Exception("No thread id in stopped event"))
    if (!running) {
      // the REPL is reading commands, it can't show the debugger prompt
      log.info(s"Thread $id stopped while no command is running, resuming it")
      await(core.continueThreads(Seq(id)))
    } else {
      val reason = body.flatMap(_.value.get("reason")).collect { case ujson.Str(s) => s }
      val steppedOut = body.flatMap(_.value.get("steppedOut")).contains(ujson.True)
      new Prompt(id, if (steppedOut) Some("breakpoint()") else reason).run()
    }
  }

  /** Name of the frame the last stop was in */
  @volatile private var lastStopFrameName = Option.empty[String]

  /** The number of the breakpoint a frame is stopped at, if it's one of ours */
  private def hitBreakpoint(f: ujson.Obj): Option[Int] =
    for {
      (name, path) <- sourceOf(f)
      line <- frameLine(f)
      (id, _) <- breakpoints.find {
        case (_, l: Location.Input) => l.line == line && s"${l.wrapperName}.sc" == name
        case (_, l: Location.File) => l.line == line && l.path == path
        case (_, l: Location.Class) =>
          l.line == line && name.stripSuffix(".scala").stripSuffix(".java") ==
            l.className.split('.').last.takeWhile(_ != '$')
      }
    } yield id

  // Sources

  private val sourceCache = new mutable.HashMap[String, Vector[String]]

  private def sourceLines(path: String): Option[Vector[String]] =
    sourceCache.synchronized {
      sourceCache.get(path).orElse {
        val res = SessionDebugger.readSource(path).map(_.linesIterator.toVector)
        for (l <- res) sourceCache(path) = l
        res
      }
    }

  // Output

  private def printError(msg: String): Unit =
    out.println(colors().error()(msg).render)

  private def printInfo(msg: String): Unit =
    out.println(msg)

  /** The prompt shown while a thread is stopped */
  private final class Prompt(threadId: Long, reason: Option[String]) {

    private var frames0: Vector[ujson.Obj] = fetchFrames()
    private var current = 0
    private var listFrom = Option.empty[Int]
    private var lastCommand = ""
    private var done = false

    private def fetchFrames(): Vector[ujson.Obj] = {
      val resp = await(
        core.stackTrace(ujson.Obj("threadId" -> threadId.toDouble, "levels" -> 1000))
      )
      for (msg <- errorMessage(resp)) throw new Exception(msg)
      resp.value.get("body").flatMap(_.obj.get("stackFrames")).toVector.flatMap(_.arr).collect {
        case o: ujson.Obj => o
      }
    }

    private def frame: Option[ujson.Obj] = frames0.lift(current)

    def run(): Unit = {
      val what = reason match {
        case Some("breakpoint") =>
          val number = frame.flatMap(hitBreakpoint)
          "Breakpoint" + number.fold("")(" " + _)
        case Some("breakpoint()") => "breakpoint()"
        case Some("pause") => "Paused"
        case Some("exception") => "Exception"
        case _ => ""
      }
      for (f <- frame) {
        val name = f.value.get("name").collect { case ujson.Str(s) => s }
        if (what.nonEmpty)
          printInfo(s"$what, ${describeFrame(f)}")
        else if (name != lastStopFrameName)
          // like gdb, when a step ends in another method
          printInfo(describeFrame(f))
        lastStopFrameName = name
        printCurrentLine(f)
      }
      while (!done) {
        readLine(colors().prompt()("(debug) ").render, history.toVector) match {
          case None =>
            printInfo("continue")
            continue()
          case Some(line) =>
            val line0 = line.trim
            if (line0.nonEmpty) {
              history += line0
              lastCommand = line0
            }
            val cmd = if (line0.isEmpty) lastCommand else line0
            if (cmd.nonEmpty)
              try command(cmd)
              catch {
                case NonFatal(e) =>
                  printError(Option(e.getMessage).getOrElse(e.toString))
              }
        }
      }
    }

    private def command(cmd: String): Unit = {
      val (name, rest) = cmd.span(!_.isWhitespace) match {
        case (n, r) => (n, r.trim)
      }
      def intArg(default: Int): Int =
        if (rest.isEmpty) default
        else Try(rest.toInt).getOrElse(throw new Exception(s"Invalid number: $rest"))
      name match {
        case "c" | "cont" | "continue" => continue()
        case "n" | "next" => step("next")
        case "s" | "step" => step("stepIn")
        case "finish" | "fin" => step("stepOut")
        case "bt" | "backtrace" | "where" => backtrace()
        case "f" | "frame" => selectFrame(intArg(current))
        case "up" => selectFrame(current + intArg(1))
        case "down" => selectFrame(current - intArg(1))
        case "l" | "list" => list(rest)
        case "p" | "print" | "inspect" =>
          if (rest.isEmpty) throw new Exception("Usage: print <expression>")
          print(rest)
        case "locals" => locals()
        case "info" | "i" =>
          rest match {
            case "locals" | "args" | "local" | "variables" => locals()
            case "breakpoints" | "break" | "b" => infoBreakpoints()
            case "frame" => frame.foreach(f => printInfo(describeFrame(f)))
            case _ => throw new Exception("Usage: info locals|breakpoints|frame")
          }
        case "b" | "break" =>
          val loc = parseLocation(rest, () => currentSource())
            .fold(err => throw new Exception(err), identity)
          val id = addBreakpoint(loc)
          printBreakpoint(id, loc)
        case "d" | "delete" =>
          if (rest.isEmpty) {
            clearBreakpoints()
            printInfo("Deleted all breakpoints")
          } else if (!removeBreakpoint(intArg(0)))
            throw new Exception(s"No breakpoint number $rest")
        case "h" | "help" => printInfo(help)
        case other =>
          throw new Exception(s"""Undefined command: "$other". Try "help".""")
      }
    }

    /** The source of the selected frame, for breakpoints with no source */
    private def currentSource(): Either[String, Option[String] => Int => Location] =
      frame.flatMap(sourceOf) match {
        case Some((name, path)) =>
          val wrapper = name.stripSuffix(".sc")
          if (isInputName(wrapper)) Right(_ => line => Location.Input(wrapper, line))
          else if (path.startsWith("jar:"))
            Left("Can't set breakpoints by line in sources from JARs, use <class name>:<line>")
          else Right(_ => line => Location.File(path, line))
        case None => Left("No current source, use <source>:<line>")
      }

    private def continue(): Unit = {
      val resp = await(core.continueThreads(Seq(threadId)))
      for (msg <- errorMessage(resp)) throw new Exception(msg)
      done = true
    }

    private def step(command: String): Unit = {
      val resp = await(core.step(command, ujson.Obj("threadId" -> threadId.toDouble)))
      for (msg <- errorMessage(resp)) throw new Exception(msg)
      done = true
    }

    private def backtrace(): Unit =
      for ((f, idx) <- frames0.zipWithIndex) {
        val marker = if (idx == current) "→" else " "
        printInfo(s"$marker#$idx  ${describeFrame(f)}")
      }

    private def selectFrame(idx: Int): Unit =
      if (idx < 0 || idx >= frames0.length)
        throw new Exception(
          if (idx < 0) "Bottom (innermost) frame selected; you cannot go down."
          else "Initial frame selected; you cannot go up."
        )
      else {
        current = idx
        listFrom = None
        for (f <- frame) {
          printInfo(s"#$idx  ${describeFrame(f)}")
          printCurrentLine(f)
        }
      }

    private def list(arg: String): Unit =
      for (f <- frame; (_, path) <- sourceOf(f)) {
        val lines = sourceLines(path).getOrElse(throw new Exception(s"Source not found: $path"))
        val line = frameLine(f).getOrElse(1)
        val from =
          if (arg.nonEmpty) Try(arg.toInt).getOrElse(throw new Exception(s"Invalid line: $arg")) - 5
          else listFrom.getOrElse(line - 5)
        val from0 = math.max(1, from)
        val until = math.min(lines.length, from0 + 9)
        if (from0 > lines.length) throw new Exception(s"Line number $from0 out of range")
        for (n <- from0 to until)
          printInfo(formatLine(n, lines(n - 1), n == line))
        listFrom = Some(until + 1)
      }

    private def print(expr: String): Unit = {
      val frameId = frame.flatMap(_.value.get("id"))
      val resp = await(
        core.request(
          "evaluate",
          ujson.Obj(
            "expression" -> expr,
            "frameId" -> frameId.getOrElse(ujson.Null),
            "context" -> "repl"
          )
        )
      )
      errorMessage(resp) match {
        case Some(msg) => printError(msg)
        case None =>
          val body = resp.value.get("body").collect { case o: ujson.Obj => o }
          val result = body.flatMap(_.value.get("result")).collect { case ujson.Str(s) => s }
          val tpe = body.flatMap(_.value.get("type")).collect { case ujson.Str(s) => s }
          printInfo(
            colors().literal()(SessionDebugger.displayValue(result.getOrElse(""), tpe)).render
          )
      }
    }

    private def locals(): Unit =
      for (f <- frame; frameId <- f.value.get("id")) {
        val resp = await(core.request("scopes", ujson.Obj("frameId" -> frameId)))
        for (msg <- errorMessage(resp)) throw new Exception(msg)
        val scopes = resp.value.get("body").flatMap(_.obj.get("scopes")).toSeq.flatMap(_.arr)
        val vars = scopes.flatMap { scope =>
          scope.obj.get("variablesReference").map(_.num.toLong).filter(_ > 0).toSeq.flatMap {
            ref =>
              val resp0 =
                await(core.request("variables", ujson.Obj("variablesReference" -> ref.toDouble)))
              resp0.value.get("body").flatMap(_.obj.get("variables")).toSeq.flatMap(_.arr)
          }
        }
        // the instances of the wrapper classes of commands, that say nothing interesting
        val vars0 = vars.filter { v =>
          val name = v.obj.get("name").collect { case ujson.Str(s) => s }
          val tpe = v.obj.get("type").collect { case ujson.Str(s) => s }.getOrElse("")
          val wrapperType =
            tpe.startsWith("cmd") && tpe.drop(3).takeWhile(_ != '$').forall(_.isDigit)
          !((name.contains("this") || name.contains("MODULE$")) && wrapperType)
        }
        if (vars0.isEmpty) printInfo("No locals.")
        for (v <- vars0) {
          val name = v.obj.get("name").collect { case ujson.Str(s) => s }.getOrElse("?")
          val value = v.obj.get("value").collect { case ujson.Str(s) => s }.getOrElse("")
          val tpe = v.obj.get("type").collect { case ujson.Str(s) => s }
          val value0 = SessionDebugger.displayValue(value, tpe)
          printInfo(s"$name = ${colors().literal()(value0).render}")
        }
      }

    private def printCurrentLine(f: ujson.Obj): Unit =
      for {
        (_, path) <- sourceOf(f)
        line <- frameLine(f)
        lines <- sourceLines(path)
        text <- lines.lift(line - 1)
      } printInfo(formatLine(line, text, current = false))

    private def formatLine(n: Int, text: String, current: Boolean): String = {
      val marker = if (current) "→ " else "  "
      colors().comment()(f"$marker$n%-4d").render + " " + text
    }
  }

  /** The code at a breakpoint location, if we know it */
  private def locationCode(loc: Location): Option[String] = {
    val path = loc match {
      case l: Location.Input =>
        core.input(l.wrapperName).map(_.sourcePath).orElse {
          outputs.sources(frames().head).reverseIterator
            .find(_.path.last == s"${l.wrapperName}.sc")
            .map(_.path.toString)
        }
      case l: Location.File => Some(l.path)
      case _: Location.Class => None
    }
    path.flatMap(sourceLines).flatMap(_.lift(loc.line - 1)).map(_.trim).filter(_.nonEmpty)
  }

  private def printBreakpoint(id: Int, loc: Location): Unit = {
    val code = locationCode(loc).fold("")(c => ": " + colors().comment()(c).render)
    printInfo(s"Breakpoint $id at ${loc.describe}$code")
  }

  private def infoBreakpoints(): Unit = {
    val bps = breakpoints
    if (bps.isEmpty) printInfo("No breakpoints.")
    else
      for ((id, loc) <- bps) {
        val code = locationCode(loc).fold("")(c => "  " + colors().comment()(c).render)
        printInfo(s"$id\t${loc.describe}$code")
      }
  }

  /** Runs a `:` command typed at the REPL prompt, like `:break 3` */
  def replCommand(line: String): Unit = {
    val (name, rest) = line.trim.stripPrefix(":").span(!_.isWhitespace) match {
      case (n, r) => (n, r.trim)
    }
    try
      name match {
        case "b" | "break" =>
          val loc = parseLocation(rest).fold(err => throw new Exception(err), identity)
          val id = addBreakpoint(loc)
          printBreakpoint(id, loc)
          startInBackground()
        case "d" | "delete" =>
          if (rest.isEmpty) {
            clearBreakpoints()
            printInfo("Deleted all breakpoints")
          } else if (!Try(rest.toInt).toOption.exists(removeBreakpoint))
            printError(s"No breakpoint number $rest")
        case "breakpoints" | "info" if rest.isEmpty || rest.startsWith("b") =>
          infoBreakpoints()
        case "h" | "help" => printInfo(replHelp)
        case other => printError(s"""Unknown command ":$other", try ":help"""")
      }
    catch {
      case NonFatal(e) =>
        printError(Option(e.getMessage).getOrElse(e.toString))
    }
  }

  /** The [[DebuggerAPI]] of the REPL */
  val api: DebuggerAPI = new DebuggerAPI {
    def enabled = true
    def break(location: String): Int = {
      val loc = parseLocation(location).fold(err => throw new Exception(err), identity)
      val id = addBreakpoint(loc)
      startInBackground()
      id
    }
    def breakpoints: Seq[(Int, String)] =
      ReplDebugger.this.breakpoints.map { case (id, loc) => (id, loc.describe) }
    def delete(number: Int): Unit =
      if (!removeBreakpoint(number))
        throw new NoSuchElementException(s"No breakpoint number $number")
    def clear(): Unit = clearBreakpoints()
  }

  def shutdown(): Unit =
    core.shutdown()

  private def frameLine(f: ujson.Obj): Option[Int] =
    f.value.get("line").collect { case n: ujson.Num => n.num.toInt }.filter(_ > 0)

  /** Name and path of the source of a frame */
  private def sourceOf(f: ujson.Obj): Option[(String, String)] = {
    val source = f.value.get("source").collect { case o: ujson.Obj => o }
    val path = source.flatMap(_.value.get("path")).collect { case ujson.Str(s) => s }
    val name = source.flatMap(_.value.get("name")).collect { case ujson.Str(s) => s }
    path.map { p =>
      val name0 = name.getOrElse {
        val p0 = SessionDebugger.jarSource(p).map(_._2).getOrElse(p)
        p0.substring(math.max(p0.lastIndexOf('/'), p0.lastIndexOf(File.separatorChar)) + 1)
      }
      (name0, p)
    }
  }

  private def describeFrame(f: ujson.Obj): String = {
    val name = f.value.get("name").collect { case ujson.Str(s) => s }.getOrElse("?")
    val where = (sourceOf(f).map(_._1), frameLine(f)) match {
      case (Some(src), Some(line)) => s" at $src:$line"
      case (Some(src), None) => s" at $src"
      case _ => ""
    }
    colors().`type`()(name).render + where
  }
}

object ReplDebugger {

  /** Whether REPL input is a debugger command, like `:break 3` */
  def isCommand(code: String): Boolean =
    code.trim.startsWith(":")

  /**
   * Parser of REPL input, that also accepts debugger commands (as single statements), so that
   * the front-end lets them through
   */
  final class CommandsParser(underlying: Parser) extends Parser {
    def split(
        code: String,
        ignoreIncomplete: Boolean,
        fileName: String
    ): Option[Either[String, Seq[String]]] =
      if (isCommand(code)) Some(Right(Seq(code.trim)))
      else underlying.split(code, ignoreIncomplete, fileName)
    def parseImportHooksWithIndices(
        source: CodeSource,
        stmts: Seq[(Int, String)]
    ): (Seq[String], Seq[ImportTree]) =
      underlying.parseImportHooksWithIndices(source, stmts)
    def splitScript(
        rawCode: String,
        fileName: String
    ): Either[String, IndexedSeq[(String, Seq[String])]] =
      underlying.splitScript(rawCode, fileName)
    def scriptBlocksWithStartIndices(
        rawCode: String,
        fileName: String
    ): Either[Parser.ScriptSplittingError, Seq[Parser.ScriptBlock]] =
      underlying.scriptBlocksWithStartIndices(rawCode, fileName)
    def defaultHighlight(
        buffer: Vector[Char],
        comment: fansi.Attrs,
        `type`: fansi.Attrs,
        literal: fansi.Attrs,
        keyword: fansi.Attrs,
        notImplemented: fansi.Attrs,
        reset: fansi.Attrs
    ): Vector[Char] =
      if (isCommand(buffer.mkString)) buffer
      else
        underlying.defaultHighlight(
          buffer,
          comment,
          `type`,
          literal,
          keyword,
          notImplemented,
          reset
        )
    def isObjDef(code: String): Boolean =
      underlying.isObjDef(code)
  }

  /** Where a breakpoint is set */
  sealed abstract class Location extends Product with Serializable {
    def line: Int
    def sourceDescription: String
    def describe: String = s"$sourceDescription:$line"
  }

  object Location {

    /** A line of a REPL command, by the name of its wrapper class (like `cmd3`) */
    final case class Input(wrapperName: String, line: Int) extends Location {
      def sourceDescription = s"$wrapperName.sc"
    }

    /** A line of a class, by its fully qualified name, like `scala.collection.immutable.List` */
    final case class Class(className: String, line: Int) extends Location {
      def sourceDescription = className
    }

    /** A line of a source file */
    final case class File(path: String, line: Int) extends Location {
      def sourceDescription = path
    }
  }

  val help: String =
    """Debugger commands:
      |  c, continue           resume the thread
      |  n, next               step to the next line, over method calls
      |  s, step               step to the next line, into method calls
      |  finish                run until the current method returns
      |  bt, backtrace         print the stack of the thread
      |  frame [n], up, down   select a frame of the stack
      |  l, list [line]        list the source around the current line
      |  p, print <expr>       evaluate an expression in the selected frame
      |  info locals           print the local variables of the selected frame
      |  b, break [src:]line   set a breakpoint, like 'break 4', 'break cmd3.sc:4',
      |                        'break scala.collection.immutable.List:120'
      |  info breakpoints      list the breakpoints
      |  d, delete [n]         delete a breakpoint, or all of them
      |  Ctrl-D                same as continue
      |An empty line repeats the last command.""".stripMargin

  val replHelp: String =
    """Debugger commands (prefix them with ':' at the REPL prompt):
      |  :b, :break [src:]line   set a breakpoint: ':break 4' (line 4 of the latest command),
      |                          ':break cmd3.sc:4', ':break scala.collection.immutable.List:120'
      |  :breakpoints            list the breakpoints
      |  :d, :delete [n]         delete a breakpoint, or all of them
      |Call breakpoint() in your code to stop there. Breakpoints can also be managed via
      |repl.debugger.""".stripMargin
}
