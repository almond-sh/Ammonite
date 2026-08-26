package ammonite.compiler.internal

import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.typer.TyperPhase

object CompilerHelper {
  def frontEndPhases = List(
    List(new Parser),
    List(new TyperPhase)
  )
}
