package ammonite.compiler.internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.reporting.{Diagnostic, MessageRendering, NoExplanation}

/** The [[MessageRenderer]] for the compilers of 3.8.2 and later. */
class MessageRenderer3_8_2 extends MessageRenderer {
  private val rendering = new MessageRendering {}
  def messageAndPos(diagnostic: Diagnostic)(using Context): String =
    rendering.messageAndPos(diagnostic)
  def rendered(diagnostic: Diagnostic)(using Context): Diagnostic =
    new Diagnostic(NoExplanation(messageAndPos(diagnostic)), diagnostic.pos, diagnostic.level)
  def stripColor(text: String): String =
    rendering.stripColor(text)
}
