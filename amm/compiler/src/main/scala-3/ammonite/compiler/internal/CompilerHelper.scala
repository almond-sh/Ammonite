package ammonite.compiler.internal

import dotty.tools.dotc.core.Contexts.{Context, ctx}
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.typer.TyperPhase

object CompilerHelper {
  def frontEndPhases: List[List[Phase]] = List(
    List(new Parser),
    List(new ExperimentalWrappersPhase),
    List(new TyperPhase)
  )

  /** Runs [[ExperimentalWrappers]] on the untyped trees, before they get typed. */
  private class ExperimentalWrappersPhase extends Phase {
    def phaseName: String = "ammonite-experimental-wrappers"
    protected def run(using Context): Unit = {
      val unit = ctx.compilationUnit
      unit.untpdTree = CompilerInternals.experimentalWrappers.markExperimental(unit.untpdTree)
    }
  }
}
