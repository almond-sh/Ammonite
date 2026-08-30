package ammonite.compiler.internal

import dotty.ammonite.compiler.AmmCompletion3_9_0
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.interactive.Completion
import dotty.tools.dotc.util.SourcePosition

/** The [[CompletionMaker]] for the compilers of 3.9.0 and later. */
class CompletionMaker3_9_0 extends CompletionMaker {
  def completions(
      pos: SourcePosition,
      dependencyCompleteOpt: Option[String => (Int, Seq[String])],
      enableDeep: Boolean
  )(using Context): (Int, List[Completion]) =
    AmmCompletion3_9_0.completions(pos, dependencyCompleteOpt, enableDeep)
}
