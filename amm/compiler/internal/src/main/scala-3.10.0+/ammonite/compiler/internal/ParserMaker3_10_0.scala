package ammonite.compiler.internal

import dotty.tools.dotc.AmmoniteFuel3_10_0
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.parsing.Scanners.Scanner
import dotty.tools.dotc.util.SourceFile

/** The [[ParserMaker]] for the compilers of 3.10.0 and later. */
class ParserMaker3_10_0 extends ParserMaker {
  def parsingContext(): Context = {
    val ctx = (new ContextBase).initialCtx
    AmmoniteFuel3_10_0.fill(ctx)
    ctx
  }
  def parser(source: SourceFile)(using Context): DottyParser =
    new Parser(source) with DottyParser
  def scanner(source: SourceFile)(using Context): Scanner =
    new Scanner(source)
}
