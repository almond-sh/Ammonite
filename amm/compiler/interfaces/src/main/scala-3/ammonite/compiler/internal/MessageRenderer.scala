package ammonite.compiler.internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.reporting.Diagnostic

/**
 * Renders a diagnostic the way the compiler's own front end does, position and all.
 *
 * This wraps a subclass of `MessageRendering`, which is a trait: the mixin forwarders such a
 * subclass carries only resolve against the compiler it was compiled with, and 3.8.2 dropped
 * `inlinePosStack` from it. 3.8.2 also gave `NoExplanation` an extra constructor parameter.
 */
trait MessageRenderer {

  /** `diagnostic`, rendered with the source it points at. */
  def messageAndPos(diagnostic: Diagnostic)(using Context): String

  /** `diagnostic`, with its message replaced by [[messageAndPos]] of it. */
  def rendered(diagnostic: Diagnostic)(using Context): Diagnostic

  /** `text`, with the ANSI colour codes the renderer may have put in taken back out. */
  def stripColor(text: String): String
}
