package ammonite.compiler.internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.TermRef

/** The [[TermRefNames]] for the compilers of 3.0.0 to 3.8.1. */
class TermRefNames3_0_0__3_8_1 extends TermRefNames {
  def designatorName(ref: TermRef)(using Context): Option[String] =
    ref.designator match {
      case n: Name => Some(n.decode.toString)
      case s: Symbol => Some(s.name.decode.toString)
      case _ => None
    }
}
