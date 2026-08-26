package ammonite.compiler.internal

import dotty.ammonite.compiler.AmmCompletion3_0_0__3_6
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.interactive.Completion
import dotty.tools.dotc.util.SourcePosition

/** The [[CompletionMaker]] for the compilers of 3.0.0 to 3.6.x. */
class CompletionMaker3_0_0__3_6 extends CompletionMaker {
  def completions(
      pos: SourcePosition,
      dependencyCompleteOpt: Option[String => (Int, Seq[String])],
      enableDeep: Boolean
  )(using Context): (Int, List[Completion]) =
    AmmCompletion3_0_0__3_6.completions(pos, dependencyCompleteOpt, enableDeep)
}
