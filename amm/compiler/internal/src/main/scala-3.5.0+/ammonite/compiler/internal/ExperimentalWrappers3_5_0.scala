package ammonite.compiler.internal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context

/** The [[ExperimentalWrappers]] for the compilers of 3.5.0 and later, which do it themselves. */
class ExperimentalWrappers3_5_0 extends ExperimentalWrappers {
  def markExperimental(tree: untpd.Tree)(using Context): untpd.Tree =
    tree
}
