package ammonite.compiler.internal

import dotty.tools.dotc.ast.{desugar, untpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.SimpleName

/** The [[GivenNames]] for the compilers of 3.0.0 to 3.4.x. */
class GivenNames3_0_0__3_4 extends GivenNames {
  def inventGivenName(tpt: untpd.Tree)(using Context): SimpleName =
    desugar.inventGivenOrExtensionName(tpt)
}
