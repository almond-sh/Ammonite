package ammonite.compiler.internal

import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.parsing.Scanners.Scanner
import dotty.tools.dotc.util.SourceFile

/** The [[ParserMaker]] for the compilers up to 3.9.x. */
class ParserMaker3_0_0__3_9 extends ParserMaker {
  def parsingContext(): Context =
    (new ContextBase).initialCtx
  def parser(source: SourceFile)(using Context): DottyParser =
    new Parser(source) with DottyParser
  def scanner(source: SourceFile)(using Context): Scanner =
    new Scanner(source)
}
