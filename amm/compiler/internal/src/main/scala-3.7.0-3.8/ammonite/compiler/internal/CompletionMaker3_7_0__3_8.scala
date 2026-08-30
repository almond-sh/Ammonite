package ammonite.compiler.internal

import dotty.ammonite.compiler.AmmCompletion3_7_0__3_8
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.interactive.Completion
import dotty.tools.dotc.util.SourcePosition

/** The [[CompletionMaker]] for the compilers of 3.7.0 to 3.8.x. */
class CompletionMaker3_7_0__3_8 extends CompletionMaker {
  def completions(
      pos: SourcePosition,
      dependencyCompleteOpt: Option[String => (Int, Seq[String])],
      enableDeep: Boolean
  )(using Context): (Int, List[Completion]) =
    AmmCompletion3_7_0__3_8.completions(pos, dependencyCompleteOpt, enableDeep)
}
