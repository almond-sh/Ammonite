package ammonite.compiler

// Originally adapted from
// https://github.com/lampepfl/dotty/blob/3.0.0-M3/
//   compiler/src/dotty/tools/dotc/printing/SyntaxHighlighting.scala

import ammonite.compiler.internal.CompilerInternals

import dotty.tools.dotc
import dotc.CompilationUnit
import dotc.ast.untpd
import dotc.core.Contexts._
import dotc.core.StdNames._
import dotc.parsing.Scanners.{InBraces, InString, Region}
import dotc.parsing.Tokens._
import dotc.reporting.Reporter
import dotc.util.Spans.Span
import dotc.util.SourceFile

import java.util.Arrays

/** This object provides functions for syntax highlighting in the REPL */
class SyntaxHighlighting(
                          noAttrs: fansi.Attrs,
                          commentAttrs: fansi.Attrs,
                          keywordAttrs: fansi.Attrs,
                          valDefAttrs: fansi.Attrs,
                          literalAttrs: fansi.Attrs,
                          typeAttrs: fansi.Attrs,
                          annotationAttrs: fansi.Attrs,
                          notImplementedAttrs: fansi.Attrs
                        ) {

  def highlight(in: String)(using Context): String = {
    def freshCtx = ctx.fresh.setReporter(Reporter.NoReporter)
    if (in.isEmpty || ctx.settings.color.value == "never") in
    else {
      val source = SourceFile.virtual("<highlighting>", in)

      given Context = freshCtx
        .setCompilationUnit(CompilationUnit(source, mustExist = false)(using freshCtx))

      val colors = Array.fill(in.length)(0L)

      def highlightRange(from: Int, to: Int, attr: fansi.Attrs) =
        Arrays.fill(colors, from, to, attr.applyMask)

      def highlightPosition(span: Span, attr: fansi.Attrs) =
        if (span.exists && span.start >= 0 && span.end <= in.length)
          highlightRange(span.start, span.end, attr)

      // Unclosed string literals don't get a STRINGLIT token: for single-line ones, the
      // scanner emits an ERROR token (a SEMI one since Scala 3.10, that recovers as if
      // the line ended there), and it stops with an early EOF for multi-line ones.
      // Such tokens are string literal (parts) if they start with a quote, or if they were
      // read as the continuation of an interpolated string (InString region) - actual
      // semi-colons never are.
      def isOpenStringLiteral(start: Int, readInString: Boolean) =
        readInString || (start < in.length && in.charAt(start) == '"')
      def endOfLine(from: Int) = {
        var idx = from
        while (idx < in.length && in.charAt(idx) != '\n' && in.charAt(idx) != '\r')
          idx += 1
        idx
      }

      val scanner = CompilerInternals.parserMaker.scanner(source)
      // The region the token following the current one is going to be read in.
      // Like Scanner.adjustSepRegions does, a closing brace leaves its InBraces region,
      // which brings us back in the enclosing interpolated string for `s"${x} foo"`.
      def nextTokenRegion(): Region | Null =
        if (scanner.token == RBRACE) {
          var region: Region | Null = scanner.currentRegion
          while (region != null && !region.isInstanceOf[InBraces])
            region = region.outer
          if (region == null) scanner.currentRegion else region.outer
        }
        else scanner.currentRegion
      // whether the current token was read while in an interpolated string
      var tokenReadInString = false
      while (scanner.token != EOF) {
        val start = scanner.offset
        val token = scanner.token
        val name = scanner.name
        val isSoftModifier = scanner.isSoftModifierInModifierPosition
        val readInString = tokenReadInString
        tokenReadInString = nextTokenRegion().isInstanceOf[InString]
        scanner.nextToken()
        val end = scanner.lastOffset

        // Branch order is important. For example,
        // `true` is at the same time a keyword and a literal
        token match {
          case _ if literalTokens.contains(token) =>
            highlightRange(start, end, literalAttrs)

          case STRINGPART =>
            // String interpolation parts include `$` but
            // we don't highlight it, hence the `-1`
            highlightRange(start, end - 1, literalAttrs)

          case _ if alphaKeywords.contains(token) || isSoftModifier =>
            highlightRange(start, end, keywordAttrs)

          case IDENTIFIER if name == nme.??? =>
            highlightRange(start, end, notImplementedAttrs)

          case ERROR | SEMI if isOpenStringLiteral(start, readInString) =>
            // single-line literal, the scanner stopped at the end of the line
            highlightRange(start, endOfLine(start), literalAttrs)

          case _ =>
        }
      }

      // unclosed multi-line string literal, running until the end of the input
      if (scanner.offset < in.length && isOpenStringLiteral(scanner.offset, tokenReadInString))
        highlightRange(scanner.offset, in.length, literalAttrs)

      for {
        comment <- scanner.comments
        span = comment.span
      } highlightPosition(span, commentAttrs)

      object TreeHighlighter extends untpd.UntypedTreeTraverser {
        import untpd._

        def ignored(tree: NameTree) = {
          val name = tree.name.toTermName
          // trees named <error> and <init> have weird positions
          name == nme.ERROR || name == nme.CONSTRUCTOR
        }

        def highlightAnnotations(tree: MemberDef): Unit =
          for (annotation <- tree.mods.annotations)
            highlightPosition(annotation.span, annotationAttrs)

        def highlight(trees: List[Tree])(using Context): Unit =
          trees.foreach(traverse)

        def traverse(tree: Tree)(using Context): Unit = {
          tree match {
            case tree: NameTree if ignored(tree) =>
              ()
            case tree: ValOrDefDef =>
              highlightAnnotations(tree)
              highlightPosition(tree.nameSpan, valDefAttrs)
            case tree: MemberDef /* ModuleDef | TypeDef */ =>
              highlightAnnotations(tree)
              highlightPosition(tree.nameSpan, typeAttrs)
            case tree: Ident if tree.isType =>
              highlightPosition(tree.span, typeAttrs)
            case _: TypTree =>
              highlightPosition(tree.span, typeAttrs)
            case _ =>
          }
          traverseChildren(tree)
        }
      }

      val parser = CompilerInternals.parserMaker.parser(source)
      val trees = parser.blockStatSeq(outermost = true)
      TreeHighlighter.highlight(trees)

      // if (colorAt.last != NoColor)
      //   highlighted.append(NoColor)

      fansi.Str.fromArrays(in.toCharArray, colors).render
    }
  }
}
