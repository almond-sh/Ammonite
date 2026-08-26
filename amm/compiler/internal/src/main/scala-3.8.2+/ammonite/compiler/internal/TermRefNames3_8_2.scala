package ammonite.compiler.internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.TermRef

/** The [[TermRefNames]] for the compilers of 3.8.2 and later. */
class TermRefNames3_8_2 extends TermRefNames {
  def designatorName(ref: TermRef)(using Context): Option[String] =
    ref.designator match {
      case n: Name => Some(n.decode.toString)
      case s: Symbol => Some(s.name.decode.toString)
      case _ => None
    }
}
