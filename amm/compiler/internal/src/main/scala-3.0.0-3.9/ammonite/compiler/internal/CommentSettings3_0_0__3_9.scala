package ammonite.compiler.internal

import dotty.tools.dotc.core.Contexts.{Context, FreshContext}

/** The [[CommentSettings]] for the compilers up to 3.9.x. */
class CommentSettings3_0_0__3_9 extends CommentSettings {
  def dropComments(using Context): Boolean =
    summon[Context].settings.YdropComments.value
  def cookComments(ctx: FreshContext): Unit =
    ctx.setSetting(ctx.settings.YcookComments, true)
}
