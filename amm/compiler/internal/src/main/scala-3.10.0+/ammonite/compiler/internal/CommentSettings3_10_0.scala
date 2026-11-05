package ammonite.compiler.internal

import dotty.tools.dotc.core.Contexts.{Context, FreshContext}

/** The [[CommentSettings]] for the compilers of 3.10.0 and later. */
class CommentSettings3_10_0 extends CommentSettings {
  def dropComments(using Context): Boolean =
    summon[Context].settings.XdropComments.value
  def cookComments(ctx: FreshContext): Unit =
    ctx.setSetting(ctx.settings.XcookComments, true)
}
