package ammonite.compiler.internal

import dotty.tools.dotc.ast.{desugar, untpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.SimpleName

/** The [[GivenNames]] for the compilers of 3.5.0 and later. */
class GivenNames3_5_0 extends GivenNames {
  def inventGivenName(tpt: untpd.Tree)(using Context): SimpleName =
    desugar.inventGivenName(tpt)
}
