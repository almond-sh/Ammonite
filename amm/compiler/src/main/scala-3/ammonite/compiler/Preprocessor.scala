package ammonite.compiler

import java.util.function.{Function => JFunction}

import ammonite.compiler.iface.{Compiler => _, Parser => _, Preprocessor => IPreprocessor, _}
import ammonite.compiler.internal.CompilerInternals
import ammonite.util.{Imports, Name, Res}
import ammonite.util.Util.CodeSource
import pprint.Util

import dotty.tools.dotc
import dotc.CompilationUnit
import dotc.ast.desugar
import dotc.ast.untpd
import dotc.core.Contexts._
import dotc.core.{Flags, Names}
import dotc.parsing.Tokens
import dotc.util.SourceFile

class Preprocessor(
  ctx: Context,
  markGeneratedSections: Boolean
) extends IPreprocessor {

  // FIXME Quite some duplication with DefaultProcessor for Scala 2.x

  /**
   * @param hoisted the language imports of the code, removed from `code`, that have to go
   *                at the top level of the generated code (see [[LanguageImports]])
   */
  private case class Expanded(code: String, printer: Seq[String], hoisted: Seq[String] = Nil)

  private def parse(source: String): Either[Seq[String], List[untpd.Tree]] = {
    val reporter = Compiler.newStoreReporter()
    val sourceFile = SourceFile.virtual("foo", source)
    // A compilation unit of its own, that parsing language imports can write to
    // (source version, capture checking, ...)
    val parseCtx0 = ctx.fresh.setReporter(reporter)
    val parseCtx = parseCtx0
      .setCompilationUnit(CompilationUnit(sourceFile, mustExist = false)(using parseCtx0))
      .withSource(sourceFile)
    val parser = CompilerInternals.parserMaker.parser(sourceFile)(using parseCtx)
    val stats = parser.blockStatSeq(outermost = true)
    parser.accept(Tokens.EOF)
    if (reporter.hasErrors) {
      val errorsStr = reporter
        .allErrors
        // .map(rendering.formatError)
        .map(e => scala.util.Try(e.msg.toString).toOption.getOrElse("???"))
      Left(errorsStr)
    } else
      Right(stats)
  }

  def transform(
    stmts: Seq[String],
    resultIndex: String,
    leadingSpaces: String,
    codeSource: CodeSource,
    indexedWrapper: Name,
    imports: Imports,
    printerTemplate: String => String,
    extraCode: String,
    skipEmpty: Boolean,
    markScript: Boolean,
    codeWrapper: CodeWrapper
  ): Res[IPreprocessor.Output] = {

    // println(s"transformOrNull(${stmts.toSeq})")

    expandStatements(stmts, resultIndex, skipEmpty).map {
      case Expanded(code, printer, hoisted) =>
        val userCode = leadingSpaces + code
        // Language imports go at the top level of the generated code, rather than in the
        // wrapper the code wrapper puts the other ones in
        val (languageImports, otherImports) =
          imports.value.partition(LanguageImports.isPersistentLanguageImport)
        // The language imports of the previous cells are marked, so that the compiler doesn't
        // report again the warnings they get in each later cell, like deprecation ones
        // (see Compiler.carriedOverSectionStart)
        val carriedOver =
          if (languageImports.isEmpty) ""
          else
            Compiler.carriedOverSectionStart + Imports(languageImports).toString +
              Compiler.carriedOverSectionEnd + System.lineSeparator()
        val topLevelCode =
          carriedOver + hoisted.map(_ + System.lineSeparator()).mkString
        val (wrappedCode, importsLength, userCodeNestingLevel) = wrapCode(
          codeSource, indexedWrapper, userCode,
          printerTemplate(printer.mkString(", ")),
          Imports(otherImports), topLevelCode, extraCode, markScript, codeWrapper
        )
        IPreprocessor.Output(
          wrappedCode,
          importsLength,
          userCodeNestingLevel,
          wrappedCode.length - importsLength - userCode.length
        )
    }
  }

  private def expandStatements(
    stmts: Seq[String],
    wrapperIndex: String,
    skipEmpty: Boolean
  ): Res[Expanded] =
    stmts match{
      // In the REPL, we do not process empty inputs at all, to avoid
      // unnecessarily incrementing the command counter
      //
      // But in scripts, we process empty inputs and create an empty object,
      // to ensure that when the time comes to cache/load the class it exists
      case Nil if skipEmpty => Res.Skip
      case postSplit =>
        Res(complete(stmts.mkString(""), wrapperIndex, postSplit))

    }

  private def wrapCode(
    codeSource: CodeSource,
    indexedWrapperName: Name,
    code: String,
    printCode: String,
    imports: Imports,
    topLevelCode: String,
    extraCode: String,
    markScript: Boolean,
    codeWrapper: CodeWrapper
  ) = {

    //we need to normalize topWrapper and bottomWrapper in order to ensure
    //the snippets always use the platform-specific newLine
    val extraCode0 =
      if (markScript) extraCode + "/*</generated>*/"
      else extraCode
    val (topWrapper1, bottomWrapper, userCodeNestingLevel) =
      codeWrapper(code, codeSource, imports, printCode, indexedWrapperName, extraCode0)
    // The code wrappers start with the package clauses, which have to stay first
    val topWrapper =
      if (topLevelCode.isEmpty) topWrapper1
      else {
        val packageDirectives = CodeWrapper.packageDirectives(codeSource.pkgName)
        if (topWrapper1.startsWith(packageDirectives))
          packageDirectives + topLevelCode + topWrapper1.stripPrefix(packageDirectives)
        else
          topLevelCode + topWrapper1
      }
    val (topWrapper0, bottomWrapper0) =
      if (markScript) (topWrapper + "/*<script>*/", "/*</script>*/ /*<generated>*/" + bottomWrapper)
      else (topWrapper, bottomWrapper)
    val importsLen = topWrapper0.length

    (topWrapper0 + code + bottomWrapper0, importsLen, userCodeNestingLevel)
  }

  // Large parts of the logic below is adapted from DefaultProcessor,
  // the Scala 2 counterpart of this file.

  private def isPrivate(tree: untpd.Tree): Boolean =
    tree match {
      case m: untpd.MemberDef => m.mods.is(Flags.Private)
      case _ => false
    }

  private def Processor(cond: PartialFunction[(String, String, untpd.Tree), Expanded]) =
    (code: String, name: String, tree: untpd.Tree) => cond.lift(name, code, tree)

  // The two empty strings passed to printTyped are replaced by AmmonitePhase,
  // with the type of ident rendered by the compiler, and the names of its fields
  // if it's a named tuple (see ResultTypes).
  private def pprintSignature(ident: String, customMsg: Option[String]): String =
    val customCode = customMsg.fold("_root_.scala.None")(x => s"""_root_.scala.Some("$x")""")
    s"""
    _root_.ammonite
          .repl
          .ReplBridge
          .value
          .Internal
          .printTyped($ident, ${Util.literalize(ident)}, $customCode, "", "")
    """
  private def definedStr(definitionLabel: String, name: String) =
    s"""
    _root_.ammonite
          .repl
          .ReplBridge
          .value
          .Internal
          .printDef("$definitionLabel", ${Util.literalize(name)})
    """
  private def pprint(ident: String) = pprintSignature(ident, None)

  /**
   * Processors for declarations which all have the same shape
   */
  private def DefProc(definitionLabel: String)(cond: PartialFunction[untpd.Tree, Names.Name]) =
    (code: String, name: String, tree: untpd.Tree) =>
      cond.lift(tree).map{ name =>
        val printer =
          if (isPrivate(tree)) Nil
          else
            val definedName =
              if name.isEmpty then ""
              else Name.backtickWrap(name.decode.toString)
            Seq(definedStr(definitionLabel, definedName))
        Expanded(
          code,
          printer
        )
      }

  private val ObjectDef = DefProc("object"){case m: untpd.ModuleDef => m.name}
  private val EnumDef = DefProc("enum"){
    case m: untpd.TypeDef if m.isClassDef && m.mods.flags.is(Flags.Enum) =>
      m.name
  }
  private val ClassDef = DefProc("class"){
    case m: untpd.TypeDef if m.isClassDef && !m.mods.flags.is(Flags.Trait) =>
      m.name
  }
  private val TraitDef = DefProc("trait"){
    case m: untpd.TypeDef if m.isClassDef && m.mods.flags.is(Flags.Trait) =>
      m.name
  }
  private val DefDef = DefProc("function"){
    case m: untpd.DefDef if m.mods.flags.is(Flags.Given) && m.name.isEmpty =>
      given Context = ctx
      CompilerInternals.givenNames.inventGivenName(m.tpt)
    case m: untpd.DefDef =>
      m.name
  }

  // `defined extension method shout`, `defined extension methods ident, concat`
  private val ExtDef = Processor { case (_, code, ext: untpd.ExtMethods) =>
    val names = ext.methods.collect {
      case m: untpd.DefDef if !isPrivate(m) => Name.backtickWrap(m.name.decode.toString)
    }
    val printer =
      if (names.isEmpty) Nil
      else {
        val label = if (names.lengthCompare(1) == 0) "extension method" else "extension methods"
        Seq(definedStr(label, names.mkString(", ")))
      }
    Expanded(code, printer)
  }
  private val TypeDef = DefProc("type"){ case m: untpd.TypeDef => m.name }

  private val VarDef = Processor { case (name, code, t: untpd.ValDef) =>
    Expanded(
      //Only wrap rhs in function if it is not a function
      //Wrapping functions causes type inference errors.
      code,
      // Try to leave out all synthetics; we don't actually have proper
      // synthetic flags right now, because we're dumb-parsing it and not putting
      // it through a full compilation
      if (isPrivate(t) || t.name.decode.toString.contains("$")) Nil
      else if (t.mods.flags.is(Flags.Given)) {
        given Context = ctx
        val name0 =
          if (t.name.isEmpty) CompilerInternals.givenNames.inventGivenName(t.tpt)
          else t.name
        Seq(pprintSignature(Name.backtickWrap(name0.decode.toString), Some("<given>")))
      }
      else if (t.mods.flags.is(Flags.Lazy))
        Seq(pprintSignature(Name.backtickWrap(t.name.decode.toString), Some("<lazy>")))
      else Seq(pprint(Name.backtickWrap(t.name.decode.toString)))
    )
  }

  private val PatDef = Processor { case (name, code, t: untpd.PatDef) =>
    val isLazy = t.mods.flags.is(Flags.Lazy)
    val printers =
      if (isPrivate(t)) Nil
      else
        t.pats
          .flatMap {
            case untpd.Tuple(trees) => trees
            case elem => List(elem)
          }
          .flatMap {
            case untpd.Ident(name) =>
              val decoded = name.decode.toString
              if (decoded.contains("$")) Nil
              else if (isLazy) Seq(pprintSignature(Name.backtickWrap(decoded), Some("<lazy>")))
              else Seq(pprint(Name.backtickWrap(decoded)))
            case _ => Nil // can this happen?
          }
    Expanded(code, printers)
  }

  private val Import = Processor {
    case (name, code, tree: untpd.Import) =>
      val Array(keyword, body) = code.split(" ", 2)
      val tq = "\"\"\""
      Expanded(code, Seq(
        s"""
        _root_.ammonite
              .repl
              .ReplBridge
              .value
              .Internal
              .printImport(${Util.literalize(body.trim)})
        """
      ))
  }

  private val Export = Processor {
    case (name, code, tree: untpd.Export) =>
      val Array(keyword, body) = code.split(" ", 2)
      Expanded(code, Seq(
        s"""
        _root_.ammonite
              .repl
              .ReplBridge
              .value
              .Internal
              .printExport(${Util.literalize(body.trim)})
        """
      ))
  }

  private val Expr = Processor {
    //Expressions are lifted to anon function applications so they will be JITed
    case (name, code, tree) =>
      val expandedCode =
        if (markGeneratedSections)
          s"/*<amm>*/val $name = /*</amm>*/$code"
        else
          s"val $name = $code"
      Expanded(
        expandedCode,
        if (isPrivate(tree)) Nil else Seq(pprint(name))
      )
  }

  /**
   * Removes the language imports of `trees`, the trees of `code`, from `code`, and renders them
   * so that they can be put at the top level of the generated code.
   *
   * The code of the imports is blanked out rather than removed, so that the positions of the
   * code that follows, in error messages in particular, don't change.
   */
  private def hoistLanguageImports(code: String, trees: List[untpd.Tree]): (String, Seq[String]) = {
    given Context = ctx
    val (languageImports, others) = trees.partition(LanguageImports.isLanguageImport)
    if (languageImports.isEmpty) (code, Nil)
    else {
      // From the expression of the import, which leaves the `import` keyword out of the
      // first of the imports of `import a.b, c.d`, to its last selector
      def importCode(tree: untpd.Tree): String =
        tree match {
          case i: untpd.Import => code.substring(i.expr.span.start, i.span.end)
          case _ => sys.error(s"Not an import: $tree") // can't happen
        }
      val hoisted = languageImports.map("import " + importCode(_))
      val blanked = code.map(c => if (c == '\n' || c == '\r') c else ' ')
      val updatedCode =
        // An empty statement where the import was, rather than just whitespace, so that the
        // code that follows the import doesn't get to be the first thing in the wrapper body,
        // whose indentation would be taken as the one of the body.
        if (others.isEmpty) ";" + blanked.drop(1)
        else {
          // only in the import clauses mixing language imports and other ones, like
          // `import scala.language.experimental.captureChecking, scala.util.Try`
          val kept = "import " + others.map(importCode).mkString(", ")
          if (code.take(kept.length).exists(c => c == '\n' || c == '\r'))
            kept + blanked.filter(c => c == '\n' || c == '\r')
          else
            kept + blanked.drop(kept.length)
        }
      (updatedCode, hoisted)
    }
  }

  private val decls = Seq[(String, String, untpd.Tree) => Option[Expanded]](
    ObjectDef, EnumDef, ClassDef, TraitDef, DefDef, ExtDef, TypeDef, VarDef, PatDef, Import, Export,
    Expr
  )

  private def complete(
    code: String,
    resultIndex: String,
    postSplit: Seq[String]
  ): Either[String, Expanded] = {
    val reParsed = postSplit.map(p => (parse(p), p))
    val errors = reParsed.collect{case (Left(e), _) => e }.flatten
    if (errors.length != 0) Left(errors.mkString(System.lineSeparator()))
    else {
      val allDecls = 
        reParsed.zipWithIndex.collect{
          case ((Right(trees), code), i) if trees.nonEmpty =>
            // Suffix the name of the result variable with the index of
            // the tree if there is more than one statement in this command
            val suffix = if (reParsed.length > 1) "_" + i else ""
            def handleTree(t: untpd.Tree) = {
              // println(s"handleTree($t)")
              val it = decls.iterator.flatMap(_.apply(code, "res" + resultIndex + suffix, t))
              if (it.hasNext)
                it.next()
              else {
                sys.error(s"Don't know how to handle ${t.getClass}: $t")
              }
            }
            val (updatedCode, hoisted) = hoistLanguageImports(code, trees)
            val expanded = trees match {
              case Seq(tree) => handleTree(tree)

              // This handles the multi-import case `import a.b, c.d`
              case trees if trees.forall(_.isInstanceOf[untpd.Import]) => handleTree(trees(0))
              // and the multi-export one, `export a.b, c.d`
              case trees if trees.forall(_.isInstanceOf[untpd.Export]) => handleTree(trees(0))

              // AFAIK this can only happen for pattern-matching multi-assignment,
              // which for some reason parse into a list of statements. In such a
              // scenario, aggregate all their printers, but only output the code once
              case trees =>
                val printers = for {
                  tree <- trees
                  if tree.isInstanceOf[untpd.ValDef]
                  Expanded(_, printers, _) = handleTree(tree)
                  printer <- printers
                } yield printer

                Expanded(code, printers)
            }
            // The trees whose code we hoisted the language imports of only consist of imports,
            // whose code `expanded` keeps as is
            if (hoisted.isEmpty) expanded
            else expanded.copy(code = updatedCode, hoisted = hoisted)
      }

      val expanded = allDecls match{
        case Seq(first, rest@_*) =>
          val allDeclsWithComments = first +: rest
          allDeclsWithComments.reduce { (a, b) =>
            Expanded(
              // We do not need to separate the code with our own semi-colons
              // or newlines, as each expanded code snippet itself comes with
              // it's own trailing newline/semicolons as a result of the
              // initial split
              a.code + b.code,
              a.printer ++ b.printer,
              a.hoisted ++ b.hoisted
            )
          }
        case Nil => Expanded("", Nil)
      }

      Right(expanded)
    }
  }
}
