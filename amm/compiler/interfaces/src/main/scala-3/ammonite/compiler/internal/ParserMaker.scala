package ammonite.compiler.internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.parsing.Scanners.Scanner
import dotty.tools.dotc.util.SourceFile

/**
 * Creates the parsers and scanners Ammonite runs on user code, and the context they run in.
 *
 * `Parser` and `Scanner` took an extra `limit` constructor parameter in 3.10.0, which also
 * bounded the depth of recursive operations by the "fuel" `ContextBase#initialize` sets up.
 */
trait ParserMaker {

  /**
   * A context to parse code in, independent from any class path.
   *
   * Unlike the contexts of compilers, its `ContextBase` isn't initialized, as that would load
   * the standard library.
   */
  def parsingContext(): Context

  /** A parser of `source`. */
  def parser(source: SourceFile)(using Context): DottyParser

  /** A scanner of `source`, starting from its beginning. */
  def scanner(source: SourceFile)(using Context): Scanner
}
