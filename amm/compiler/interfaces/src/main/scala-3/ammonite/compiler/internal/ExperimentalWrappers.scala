package ammonite.compiler.internal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context

/**
 * Lets experimental language imports at the top level of a compilation unit apply to it.
 *
 * Since 3.5.0, such an import makes the compiler mark all the top-level definitions of the
 * unit `@experimental`. Before that, the compiler only accepts these imports if all those
 * definitions are `@experimental` already - which those of the code wrappers aren't.
 */
trait ExperimentalWrappers {

  /**
   * `tree`, the untyped tree of a whole compilation unit, with the top-level definitions of
   * its packages that have experimental language imports marked `@experimental`, if the
   * compiler doesn't do so itself.
   */
  def markExperimental(tree: untpd.Tree)(using Context): untpd.Tree
}
