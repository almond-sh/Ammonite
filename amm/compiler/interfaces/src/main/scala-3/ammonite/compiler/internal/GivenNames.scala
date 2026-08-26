package ammonite.compiler.internal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.SimpleName

/**
 * Names the compiler invents for anonymous givens.
 *
 * `desugar.inventGivenOrExtensionName` became `desugar.inventGivenName` in 3.5.0.
 */
trait GivenNames {

  /** The name the compiler would give an anonymous `given` of type `tpt`. */
  def inventGivenName(tpt: untpd.Tree)(using Context): SimpleName
}
