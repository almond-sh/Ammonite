package dotty.tools.dotc

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.RecursiveOperation

/**
 * Sets up the fuel of the recursive operations of a context whose `ContextBase` isn't
 * initialized, like `ContextBase#initialize` does, for the compilers of 3.10.0 and later.
 *
 * Until then, a `ContextBase` only has enough fuel for trivial things, and parsing nested
 * code with it fails with a `RecursionOverflow`. What it is made of is `private[dotc]`, hence
 * the package of this.
 */
object AmmoniteFuel3_10_0 {
  def fill(ctx: Context): Unit = {
    val base = ctx.base
    base.recursiveOperations =
      Array.fill[RecursiveOperation](ctx.settings.XmaxFuel.value(using ctx))(RecursiveOperation.blank())
    base.recursiveDepth =
      if (ctx.settings.XnoEnrichErrorMessages.value(using ctx)) Int.MinValue else 0
  }
}
