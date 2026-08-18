package dotty.ammonite.compiler

import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.interactive.Completion

trait AmmCompletionVersionSpecific {
  def scopeCompletionNames(
    completer: Completion.Completer
  ): Map[Name, Seq[SingleDenotation]] =
    completer.scopeCompletions.names
}
