package ammonite.compiler

import java.net.URL
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.io.{ByteArrayInputStream, OutputStream}

import ammonite.compiler.iface.{
  Compiler => ICompiler,
  CompilerBuilder => ICompilerBuilder,
  CompilerLifecycleManager => ICompilerLifecycleManager,
  Preprocessor => IPreprocessor,
  _
}
import ammonite.compiler.internal.{CompilerHelper, CompilerInternals}
import ammonite.util.{ImportData, Imports, PositionOffsetConversion, Printer}
import ammonite.util.Util.newLine

import dotty.tools.dotc
import dotc.{CompilationUnit, Compiler => DottyCompiler, Run, ScalacCommand}
import dotc.ast.{tpd, untpd}
import dotc.ast.Positioned
import dotc.config.CompilerCommand
import dotc.core.Contexts._
import dotc.core.{Flags, MacroClassLoader, Mode}
import dotc.core.Comments.{ContextDoc, ContextDocstrings}
import dotc.core.Phases.{Phase, unfusedPhases}
import dotc.core.Symbols.{defn, Symbol}
import dotc.fromtasty.TastyFileUtil
import dotc.interactive.{Completion, Interactive}
import dotc.report
import dotc.reporting
import dotc.semanticdb
import dotc.transform.{PostTyper, Staging}
import dotc.util.{Property, SourceFile, SourcePosition}
import dotc.util.Spans.Span
import dotty.tools.io.{
  AbstractFile,
  File,
  VirtualDirectory,
  PlainFile
}

class Compiler(
  dynamicClassPath: AbstractFile,
  initialClassPath: Seq[URL],
  classPath: Seq[URL],
  macroClassLoader: ClassLoader,
  whiteList: Set[Seq[String]],
  dependencyCompleteOpt: => Option[String => (Int, Seq[String])] = None,
  contextInit: FreshContext => Unit = _ => (),
  settings: Seq[String] = Nil,
  reporter: Option[ICompilerBuilder.Message => Unit] = None
) extends ICompiler:
  self =>

  import Compiler.{enumerateVdFiles, files}

  /**
   * A new in-memory directory for the compiler to write class files to.
   *
   * We use a new one for each compilation, as `VirtualDirectory` can't be emptied since 3.10.0
   * (its constructor is `private[io]` since then too, but that's only enforced by the compiler,
   * and the one we compile against doesn't).
   */
  private def newOutputDir(): VirtualDirectory = new VirtualDirectory("(memory)")

  private def initCtx: Context =
    val base: ContextBase =
      new ContextBase:
        override protected def newPlatform(using Context) =
          CompilerInternals.platformMaker.platform(
            initialClassPath,
            self.classPath,
            dynamicClassPath,
            whiteList
          )
    base.initialCtx

  private def sourcesRequired = false

  private lazy val MacroClassLoaderKey =
    val cls = macroClassLoader.loadClass("dotty.tools.dotc.core.MacroClassLoader$")
    val fld = cls.getDeclaredField("MacroClassLoaderKey")
    fld.setAccessible(true)
    fld.get(null).asInstanceOf[Property.Key[ClassLoader]]

  // Originally adapted from
  // https://github.com/lampepfl/dotty/blob/3.0.0-M3/
  //   compiler/src/dotty/tools/dotc/Driver.scala/#L67-L81
  private def setup(args: Array[String], rootCtx: Context): (List[String], Context) =
    given ictx: FreshContext = rootCtx.fresh
    val summary = ScalacCommand.distill(args, ictx.settings)(ictx.settingsState)(using ictx)
    ictx.setSettings(summary.sstate)
    ictx.setProperty(MacroClassLoaderKey, macroClassLoader)
    Positioned.init

    if !CompilerInternals.commentSettings.dropComments then
      ictx.setProperty(ContextDoc, new ContextDocstrings)
    val fileNamesOpt = ScalacCommand.checkUsage(
      summary,
      sourcesRequired
    )(using ictx.settings)(using ictx.settingsState)
    val fileNames = fileNamesOpt.getOrElse {
      throw new Exception("Error initializing compiler")
    }
    contextInit(ictx)
    (fileNames, ictx)

  // Originally adapted from
  // https://github.com/lampepfl/dotty/blob/3.0.0-M3/
  //   compiler/src/dotty/tools/repl/ReplDriver.scala/#L67-L73
  /** Create a fresh and initialized context with IDE mode enabled */
  lazy val initialCtx =
    val rootCtx = initCtx.fresh.addMode(Mode.ReadPositions | Mode.Interactive)
    CompilerInternals.commentSettings.cookComments(rootCtx)
    // FIXME Disabled for the tests to pass
    rootCtx.setSetting(rootCtx.settings.color, "never")
    // FIXME We lose possible custom openStream implementations on the URLs of initialClassPath and
    // classPath
    val initialClassPath0 = initialClassPath
      // .filter(!_.toURI.toASCIIString.contains("fansi_2.13"))
      // .filter(!_.toURI.toASCIIString.contains("pprint_2.13"))
    rootCtx.setSetting(rootCtx.settings.outputDir, newOutputDir())

    val (_, ictx) = setup(settings.toArray, rootCtx)
    ictx.base.initialize()(using ictx)
    ictx

  private var userCodeNestingLevel = -1
  // where the user code starts in the code we compile
  private var userCodeOffset = 0

  // Originally adapted from
  // https://github.com/lampepfl/dotty/blob/3.0.0-M3/
  //   compiler/src/dotty/tools/repl/ReplCompiler.scala/#L34-L39
  val compiler =
    new DottyCompiler:
      override protected def frontendPhases: List[List[Phase]] =
        CompilerHelper.frontEndPhases ++
        List(
          List(semanticdb.AmmoniteSemanticDB.extractSemanticInfo),
          List(new AmmonitePhase(userCodeNestingLevel, userCodeNestingLevel == 2, userCodeOffset)),
          List(new PostTyper)
        )

  // Originally adapted from
  // https://github.com/lampepfl/dotty/blob/3.0.0-M3/
  //   compiler/src/dotty/tools/repl/Rendering.scala/#L97-L103
  /** Formats errors using the `messageRenderer` */
  private def formatError(dia: reporting.Diagnostic)(implicit ctx: Context): reporting.Diagnostic =
    CompilerInternals.messageRenderer.rendered(dia)

  def compile(
    src: Array[Byte],
    printer: Printer,
    importsLen: Int,
    userCodeNestingLevel: Int,
    fileName: String,
    suffixLen: Int
  ): Option[ICompiler.Output] =
    // println(s"Compiling\n${new String(src, StandardCharsets.UTF_8)}\n")

    self.userCodeNestingLevel = userCodeNestingLevel
    self.userCodeOffset = importsLen

    // Where the user code lies in src, between the code Ammonite generates around it
    val userCodeEnd = new String(src, "UTF-8").length - suffixLen
    /*
     * Whether a diagnostic is Scala 3.9's E230 warning ("The identifier ... should not contain
     * `$`") about the code Ammonite generates around the user code, like `package $sess` or
     * `object $routes`. Users can't do anything about those, so we don't show them. E230 on
     * user code, and any other diagnostic, are still reported.
     *
     * E230 is IllegalIdentifierID, matched by number as it doesn't exist in Scala 3.3 (whose
     * error numbers stop below 230), and error numbers are never reassigned.
     */
    def isGeneratedCodeWarning(diag: reporting.Diagnostic, sourceFile: SourceFile): Boolean =
      diag.level == dotc.interfaces.Diagnostic.WARNING &&
        diag.msg.errorId.errorNumber == 230 &&
        diag.pos.exists &&
        diag.pos.source == sourceFile &&
        (diag.pos.point < importsLen || diag.pos.point > userCodeEnd)
    // set below, before compiling anything
    var sourceFile0: SourceFile = null

    val reporter0 = reporter match {
      case None =>
        Compiler.newStoreReporter()
      case Some(rep) =>
        val simpleReporter = new dotc.interfaces.SimpleReporter {
          def report(diag: dotc.interfaces.Diagnostic) = diag match {
            case d: reporting.Diagnostic if isGeneratedCodeWarning(d, sourceFile0) =>
            case _ => report0(diag)
          }
          private def report0(diag: dotc.interfaces.Diagnostic) = {
            val severity = diag.level match {
              case dotc.interfaces.Diagnostic.ERROR => "ERROR"
              case dotc.interfaces.Diagnostic.WARNING => "WARNING"
              case dotc.interfaces.Diagnostic.INFO => "INFO"
              case _ => "INFO" // should not happen
            }
            val pos = Some(diag.position).filter(_.isPresent).map(_.get)
            val start = pos.fold(0)(_.start)
            val end = pos.fold(new String(src, "UTF-8").length)(_.end)
            val msg = ICompilerBuilder.Message(severity, start, end, diag.message)
            rep(msg)
          }
        }
        reporting.Reporter.fromSimpleReporter(simpleReporter)
    }
    val outputDir = newOutputDir()
    val run = new Run(
      compiler,
      initialCtx.fresh
        .setReporter(reporter0)
        .setSetting(initialCtx.settings.outputDir, outputDir)
    )

    val semanticDbEnabled = run.runContext.settings.Xsemanticdb.value(using run.runContext)
    val sourceFile =
      if (semanticDbEnabled)
        // semanticdb needs the sources to be written on disk, so we assume they're there already
        CompilerInternals.sourceFileMaker.inSourceRoot(fileName)(using run.runContext)
      else
        CompilerInternals.sourceFileMaker.virtual(fileName, src)
    sourceFile0 = sourceFile

    implicit val ctx: Context = run.runContext.withSource(sourceFile)

    val unit = CompilerInternals.compilationUnitMaker.notSuspendable(ctx.source)
    ctx
      .run
      .compileUnits(unit :: Nil)

    val result =
      if (ctx.reporter.hasErrors) Left(reporter.fold(ctx.reporter.removeBufferedMessages)(_ => Nil))
      else Right((reporter.fold(ctx.reporter.removeBufferedMessages)(_ => Nil), unit))

    def formatDiagnostics(diagnostics: List[reporting.Diagnostic]): List[String] = {
      val scalaPosToScPos = PositionOffsetConversion.scalaPosToScPos(
        new String(src).drop(importsLen),
        0,
        0,
        new String(src),
        importsLen
      )
      val scFile = CompilerInternals.sourceFileMaker.withContent(
        sourceFile,
        sourceFile.content().drop(importsLen)
      )
      def scalaOffsetToScOffset(scalaOffset: Int): Option[Int] =
        scalaPosToScPos(sourceFile.offsetToLine(scalaOffset), sourceFile.column(scalaOffset)).map {
          case (scLine, scCol) => scFile.lineToOffset(scLine) + scCol
        }
      def scalaSpanToScSpan(scalaSpan: Span): Option[Span] =
        for {
          scStart <- scalaOffsetToScOffset(scalaSpan.start)
          scEnd <- scalaOffsetToScOffset(scalaSpan.end)
          scPoint <- scalaOffsetToScOffset(scalaSpan.point)
        } yield Span(scStart, scEnd, scPoint)
      def scalaSourcePosToScSourcePos(sourcePos: SourcePosition): Option[SourcePosition] =
        if (sourcePos.source == sourceFile)
          scalaSpanToScSpan(sourcePos.span).map { scSpan =>
            SourcePosition(scFile, scSpan, sourcePos.outer)
          }
        else
          None
      def scalaDiagnosticToScDiagnostic(diag: reporting.Diagnostic): Option[reporting.Diagnostic] =
        scalaSourcePosToScSourcePos(diag.pos).map { scPos =>
          new reporting.Diagnostic(diag.msg, scPos, diag.level)
        }

      diagnostics
        .filter(d => !isGeneratedCodeWarning(d, sourceFile))
        .map(d => scalaDiagnosticToScDiagnostic(d).getOrElse(d))
        .map(formatError)
        .map(_.msg.toString)
    }

    result match {
      case Left(errors) =>
        for (err <- formatDiagnostics(errors))
          printer.error(err)
        None
      case Right((warnings, unit)) =>
        for (warn <- formatDiagnostics(warnings))
          printer.warning(warn)
        val newImports = unfusedPhases.collectFirst {
          case p: AmmonitePhase => p.importData
        }.getOrElse(Seq.empty[ImportData])
        val usedEarlierDefinitions = unfusedPhases.collectFirst {
          case p: AmmonitePhase => p.usedEarlierDefinitions
        }.getOrElse(Seq.empty[String])
        val fileCount = enumerateVdFiles(outputDir).length
        val classes = files(outputDir).toArray
        // outputDir is None here, dynamicClassPath should already correspond to an on-disk directory
        Compiler.addToClasspath(classes, dynamicClassPath, None)
        val lineShift = PositionOffsetConversion.offsetToPos(new String(src)).apply(importsLen).line
        val mappings = Map(sourceFile.file.name -> (sourceFile.file.name, -lineShift))
        val postProcessedClasses = classes.toVector.map {
          case (path, byteCode) if path.endsWith(".class") =>
            val updatedByteCodeOpt = AsmPositionUpdater.postProcess(
              mappings,
              new ByteArrayInputStream(byteCode)
            )
            (path, updatedByteCodeOpt.getOrElse(byteCode))
          case other =>
            other
        }
        val output = ICompiler.Output(
          postProcessedClasses,
          Imports(newImports),
          Some(usedEarlierDefinitions)
        )
        Some(output)
    }

  def objCompiler = compiler

  def preprocessor(fileName: String, markGeneratedSections: Boolean): IPreprocessor =
    new Preprocessor(
      initialCtx.fresh.withSource(SourceFile.virtual(fileName, "")),
      markGeneratedSections: Boolean
    )

  // Originally adapted from
  // https://github.com/lampepfl/dotty/blob/3.0.0-M3/
  //   compiler/src/dotty/tools/repl/ReplCompiler.scala/#L224-L286
  def tryTypeCheck(
    src: Array[Byte],
    fileName: String
  ) =
    val sourceFile = SourceFile.virtual(fileName, new String(src, StandardCharsets.UTF_8))

    val reporter0 = Compiler.newStoreReporter()
    val run = new Run(
      compiler,
      initialCtx.fresh
        .addMode(Mode.ReadPositions | Mode.Interactive)
        .setReporter(reporter0)
        .setSetting(initialCtx.settings.YstopAfter, List("typer"))
    )
    implicit val ctx: Context = run.runContext.withSource(sourceFile)

    val unit = CompilerInternals.compilationUnitMaker.notSuspendable(ctx.source)
    ctx
      .run
      .compileUnits(unit :: Nil, ctx)

    (unit.tpdTree, ctx)

  def complete(
    offset: Int,
    previousImports: String,
    snippet: String
  ): (Int, Seq[String], Seq[String]) = {
    val res = completeOnce(offset, previousImports, snippet)
    // Like in Scala 2, complete `import $ivy.`foo` as if its backtick was closed, so that it
    // parses as an import
    val isPartialIvyImport = res._2.isEmpty &&
      snippet.split("\\s+|`").startsWith(Seq("import", "$ivy.")) &&
      snippet.count(_ == '`') == 1
    if (isPartialIvyImport) completeOnce(offset, previousImports, snippet + "`")
    else res
  }

  private def completeOnce(
    offset: Int,
    previousImports: String,
    snippet: String
  ): (Int, Seq[String], Seq[String]) = {

    val prefix = previousImports + newLine +
      "object AutocompleteWrapper{ val expr: _root_.scala.Unit = {" + newLine
    val suffix = newLine + "()}}"
    val allCode = prefix + snippet + suffix
    val index = offset + prefix.length


    // Originally based on
    // https://github.com/lampepfl/dotty/blob/3.0.0-M1/
    //   compiler/src/dotty/tools/repl/ReplDriver.scala/#L179-L191

    val (tree, ctx0) = tryTypeCheck(allCode.getBytes("UTF-8"), "<completions>")
    val ctx = ctx0.fresh
    val file = SourceFile.virtual("<completions>", allCode, maybeIncomplete = true)
    val unit = CompilationUnit(file)(using ctx)
    unit.tpdTree = {
      given Context = ctx
      import tpd._
      tree match {
        case PackageDef(_, p) =>
          p.collectFirst {
            case TypeDef(_, tmpl: Template) =>
              tmpl.body
                .collectFirst { case dd: ValDef if dd.name.show == "expr" => dd }
                .getOrElse(???)
          }.getOrElse(???)
        case _ => ???
      }
    }
    val ctx1 = ctx.fresh.setCompilationUnit(unit)
    val srcPos = SourcePosition(file, Span(index))
    val (start, completions) = CompilerInternals.completionMaker.completions(
      srcPos,
      dependencyCompleteOpt = dependencyCompleteOpt,
      enableDeep = false
    )(using ctx1)

    val blacklistedPackages = Set("shaded")

    def deepCompletion(name: String): List[String] = {
      given Context = ctx1
      def rec(t: Symbol): Seq[Symbol] = {
        if (blacklistedPackages(t.name.toString))
          Nil
        else {
          val children =
            if (t.is(Flags.Package) || t.is(Flags.PackageVal) || t.is(Flags.PackageClass))
              t.denot.info.allMembers.map(_.symbol).filter(_ != t).flatMap(rec)
            else Nil

          t +: children.toSeq
        }
      }

      val found = for {
        member <- defn.RootClass.denot.info.allMembers.map(_.symbol).toList
        sym <- rec(member)
        // module classes come with their module values, no need to list them twice
        if !sym.is(Flags.ModuleClass)
        // Scala 2 comment: sketchy name munging because I don't know how to do this properly
        // Note lack of back-quoting support.
        strippedName = sym.name.toString.stripPrefix("package$").stripSuffix("$")
        if strippedName.startsWith(name)
        // inner and anonymous classes of Java class files
        if !strippedName.contains('$')
        (pref, _) = sym.fullName.toString.splitAt(sym.fullName.toString.lastIndexOf('.') + 1)
        out = pref + strippedName
        if out != ""
      } yield out.replace(".package$.", ".")
      found.distinct
    }

    def blacklisted(s: Symbol) = {
      given Context = ctx1
      val blacklist = Set(
        "scala.Predef.any2stringadd.+",
        "scala.Any.##",
        "java.lang.Object.##",
        "scala.<byname>",
        "scala.<empty>",
        "scala.<repeated>",
        "scala.<repeated...>",
        "scala.Predef.StringFormat.formatted",
        "scala.Predef.Ensuring.ensuring",
        "scala.Predef.ArrowAssoc.->",
        "scala.Predef.ArrowAssoc.→",
        "java.lang.Object.synchronized",
        "java.lang.Object.ne",
        "java.lang.Object.eq",
        "java.lang.Object.wait",
        "java.lang.Object.notifyAll",
        "java.lang.Object.notify",
        "java.lang.Object.clone",
        "java.lang.Object.finalize"
      )

      blacklist(s.fullName.toString) ||
      s.isOneOf(Flags.GivenOrImplicit) ||
      // Cache objects, which you should probably never need to
      // access directly, and apart from that have annoyingly long names
      "cache[a-f0-9]{32}".r.findPrefixMatchOf(s.name.decode.toString).isDefined ||
      // s.isDeprecated ||
      s.name.decode.toString == "<init>" ||
      s.name.decode.toString.contains('$')
    }

    val filteredCompletions = completions.filter { c =>
      c.symbols.isEmpty || c.symbols.exists(!blacklisted(_))
    }
    // Like in Scala 2, when an identifier matches nothing in scope, look for it in all
    // the packages and package objects we can see (`fromN` -> `scala.concurrent.duration.fromNow`)
    val deepIdentOpt =
      if (filteredCompletions.nonEmpty) None
      else {
        given Context = ctx1
        Interactive.pathTo(unit.tpdTree, srcPos.span) match {
          case (id: tpd.Ident) :: _ if id.span.exists =>
            Some((id.span.start, id.name.decode.toString))
          case _ => None
        }
      }
    deepIdentOpt match {
      case Some((identStart, name)) =>
        val deep = deepCompletion(name)
        // Like Scala 2, add an empty completion when there are several candidates, so that
        // their common prefix doesn't get inserted right away
        val names = if (deep.length <= 1) deep else deep :+ ""
        (identStart - prefix.length, names, Nil)
      case None =>
        shallowCompletions(start, prefix, ctx1, filteredCompletions)
    }
  }

  private def shallowCompletions(
    start: Int,
    prefix: String,
    ctx1: Context,
    filteredCompletions: List[Completion]
  ): (Int, Seq[String], Seq[String]) = {
    val signatures = {
      given Context = ctx1
      for {
        c <- filteredCompletions
        s <- c.symbols
        isMethod = s.denot.is(Flags.Method)
        if isMethod
      } yield s"def ${s.name}${s.denot.info.widenTermRefExpr.show}"
    }
    (start - prefix.length, filteredCompletions.map(_.label.replace(".package$.", ".")), signatures)
  }

object Compiler:

  /** Create empty outer store reporter */
  def newStoreReporter(): reporting.StoreReporter =
    new reporting.StoreReporter(null)
    with reporting.UniqueMessagePositions with reporting.HideNonSensicalMessages

  private def enumerateVdFiles(d: VirtualDirectory): Iterator[AbstractFile] =
    val (subs, files) = d.iterator.partition(_.isDirectory)
    files ++ subs.map(_.asInstanceOf[VirtualDirectory]).flatMap(enumerateVdFiles)

  private def files(d: VirtualDirectory): Iterator[(String, Array[Byte])] =
    for (x <- enumerateVdFiles(d) if x.name.endsWith(".class") || x.name.endsWith(".tasty")) yield {
      val segments = x.path.split("/").toList.tail
      (x.path.stripPrefix("(memory)/"), x.toByteArray)
    }

  private def writeDeep(
    d: AbstractFile,
    path: List[String]
  ): OutputStream = path match {
    case head :: Nil => d.fileNamed(path.head).output
    case head :: rest =>
      writeDeep(
        d.subdirectoryNamed(head), //.asInstanceOf[VirtualDirectory],
        rest
      )
    // We should never write to an empty path, and one of the above cases
    // should catch this and return before getting here
    case Nil => ???
  }

  /**
   * The output directory at `path`, which must exist, as an `AbstractFile`.
   *
   * `AbstractFile.getDirectory` takes the version of the JARs it might open since 3.10.0, which
   * we don't need for directories, so we create the `PlainFile` it would create ourselves.
   */
  def directory(path: Path): AbstractFile =
    new PlainFile(dotty.tools.io.Directory(path))

  def addToClasspath(classFiles: Traversable[(String, Array[Byte])],
                     dynamicClasspath: AbstractFile,
                     outputDir: Option[Path]): Unit = {

    for((name, bytes) <- classFiles){
      val elems = name.split('/').toList
      val output = writeDeep(dynamicClasspath, elems)
      output.write(bytes)
      output.close()

      for (dir <- outputDir)
        CompilerUtil.writeOver(dir, elems, bytes)
    }

  }

