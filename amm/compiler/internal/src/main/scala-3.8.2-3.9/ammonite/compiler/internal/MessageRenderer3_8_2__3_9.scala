package ammonite.compiler.internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.reporting.{Diagnostic, MessageRendering, NoExplanation}

/** The [[MessageRenderer]] for the compilers of 3.8.2 to 3.9.x. */
class MessageRenderer3_8_2__3_9 extends MessageRenderer {
  private val rendering = new MessageRendering {}
  def messageAndPos(diagnostic: Diagnostic)(using Context): String =
    rendering.messageAndPos(diagnostic)
  def rendered(diagnostic: Diagnostic)(using Context): Diagnostic =
    new Diagnostic(NoExplanation(messageAndPos(diagnostic)), diagnostic.pos, diagnostic.level)
  def stripColor(text: String): String =
    rendering.stripColor(text)
}
