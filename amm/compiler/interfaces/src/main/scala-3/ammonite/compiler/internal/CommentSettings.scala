package ammonite.compiler.internal

import dotty.tools.dotc.core.Contexts.{Context, FreshContext}

/**
 * The settings of the compiler about the documentation comments of the sources.
 *
 * `-Ydrop-docs` and `-Ycook-docs` became `-Xdrop-docs` and `-Xcook-docs` in 3.9.0, and the
 * former were dropped in 3.10.0.
 */
trait CommentSettings {

  /** Whether the compiler drops the documentation comments of the sources it reads. */
  def dropComments(using Context): Boolean

  /** Has the compiler type check documentation comments, `@usecase`-s and all. */
  def cookComments(ctx: FreshContext): Unit
}
