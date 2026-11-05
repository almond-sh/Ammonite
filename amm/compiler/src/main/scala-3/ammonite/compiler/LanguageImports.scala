package ammonite.compiler

import ammonite.util.{ImportData, Name}

import dotty.tools.dotc
import dotc.ast.untpd
import dotc.core.Contexts.Context

/**
 * Language imports (`import scala.language.experimental.captureChecking`,
 * `import language.implicitConversions`, ...) in the Scala 3 REPL and scripts.
 *
 * Ammonite wraps the code of each cell / script block in an object or a class, so that the
 * language imports it contains would end up in a local scope. That is not allowed for
 * experimental ones ("Experimental feature local import may only be used under experimental
 * mode") nor for the ones affecting parsing (`captureChecking`, `pureFunctions`, source
 * versions, ...: "this language import is only allowed at the toplevel"). So [[Preprocessor]]
 * hoists the language imports of a cell to the top level of the code it generates, along with
 * those of the previous cells, which keep being in effect in the later ones, like they do in
 * dotty's REPL. [[AmmonitePhase]] records them along with the other imports of the cell.
 */
object LanguageImports {

  private val languagePrefix = Seq("scala", "language")
  private val experimentalPrefix = languagePrefix :+ "experimental"

  /** Whether `prefix` is the one of a language import, like `_root_.scala.language`. */
  def isLanguagePrefix(prefix: Seq[Name]): Boolean = {
    val raw = prefix.map(_.raw) match {
      case "_root_" +: rest => rest
      case other => other
    }
    raw == languagePrefix || raw == experimentalPrefix
  }

  /** Whether `name`, imported from `scala.language`, is a source version, like `3.0-migration`. */
  def isSourceVersion(name: String): Boolean =
    name.headOption.exists(_.isDigit) || name == "future" || name == "future-migration"

  /** Whether `data`, imported from `scala.language`, is a source version import. */
  def isSourceVersionImport(data: ImportData): Boolean =
    isLanguagePrefix(data.prefix) &&
      data.prefix.lastOption.map(_.raw).contains("language") &&
      isSourceVersion(data.fromName.raw)

  /**
   * Whether `data` is a language import that stays in effect in the cells that follow the
   * one it was made in.
   *
   * Source version imports don't: they only apply to the cell they're in, and a compilation
   * unit can't have more than one of them.
   */
  def isPersistentLanguageImport(data: ImportData): Boolean =
    isLanguagePrefix(data.prefix) && !isSourceVersionImport(data)

  /** Whether `tree` is a language import, like `import language.experimental.{a, b}`. */
  def isLanguageImport(tree: untpd.Tree)(using Context): Boolean =
    tree match {
      case i: untpd.Import => untpd.languageImport(i.expr).isDefined
      case _ => false
    }
}
