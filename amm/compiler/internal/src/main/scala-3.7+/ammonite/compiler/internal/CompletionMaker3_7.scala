package ammonite.compiler.internal

import dotty.ammonite.compiler.AmmCompletion3_7
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.interactive.Completion
import dotty.tools.dotc.util.SourcePosition

/** The [[CompletionMaker]] for the compilers of 3.7.0 and later. */
class CompletionMaker3_7 extends CompletionMaker {
  def completions(
      pos: SourcePosition,
      dependencyCompleteOpt: Option[String => (Int, Seq[String])],
      enableDeep: Boolean
  )(using Context): (Int, List[Completion]) =
    AmmCompletion3_7.completions(pos, dependencyCompleteOpt, enableDeep)
}
