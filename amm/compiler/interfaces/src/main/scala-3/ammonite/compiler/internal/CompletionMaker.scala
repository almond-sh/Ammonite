package ammonite.compiler.internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.interactive.Completion
import dotty.tools.dotc.util.SourcePosition

/**
 * Completes the code at a position, the way Ammonite wants it: `$ivy` imports complete
 * against the dependency completer, and everything else against the compiler.
 *
 * `Completion.Completer` took over the whole tree path, rather than just the position, in
 * 3.7.0, and what `scopeCompletions` hands back changed along the way too.
 */
trait CompletionMaker {

  /** The offset the completions start at, and the completions themselves. */
  def completions(
      pos: SourcePosition,
      dependencyCompleteOpt: Option[String => (Int, Seq[String])],
      enableDeep: Boolean
  )(using Context): (Int, List[Completion])
}
