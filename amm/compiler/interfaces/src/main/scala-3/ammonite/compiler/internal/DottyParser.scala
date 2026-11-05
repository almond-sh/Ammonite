package ammonite.compiler.internal

import dotty.tools.dotc
import dotc.ast.untpd
import dotc.core.Flags
import dotc.core.StdNames.nme
import dotc.parsing.Parsers.{Location, Parser}
import dotc.parsing.Tokens

import scala.collection.mutable

/**
 * The dotty parser Ammonite parses user code with, which accepts things like top-level
 * `private` definitions that dotty's own parser rejects in blocks.
 *
 * A trait rather than a class, as `Parser` took more constructor parameters in 3.10.0: the
 * [[ParserMaker]] implementations mix it in the `Parser`s they create for each compiler. What
 * it calls and overrides of `Parser` hasn't changed in the Scala 3 versions we support.
 */
trait DottyParser extends Parser {

  // From
  // https://github.com/lampepfl/dotty/blob/3.0.0-M3/
  //   compiler/src/dotty/tools/dotc/parsing/Parsers.scala/#L67-L71
  extension (buf: mutable.ListBuffer[untpd.Tree])
    def +++=(x: untpd.Tree) = x match {
      case x: untpd.Thicket => buf ++= x.trees
      case x => buf += x
    }

  private val oursLocalModifierTokens = Tokens.localModifierTokens + Tokens.PRIVATE

  override def localDef(
    start: Int,
    implicitMods: untpd.Modifiers = untpd.EmptyModifiers
  ): untpd.Tree = {
    var mods = defAnnotsMods(oursLocalModifierTokens)
    for (imod <- implicitMods.mods) mods = addMod(mods, imod)
    if (mods.is(Flags.Final))
      // A final modifier means the local definition is "class-like".
      // FIXME: Deal with modifiers separately
      tmplDef(start, mods)
    else
      defOrDcl(start, mods)
  }

  // Adapted from
  // https://github.com/lampepfl/dotty/blob/3.2.0/
  //   compiler/src/dotty/tools/dotc/parsing/Parsers.scala#L4075-L4094
  // Unlike it, we accept private modifiers for top-level definitions.
  // Like dotty's own REPL, we accept `opaque type` definitions at the outermost level (dotty
  // only does so in `Mode.Interactive`, we do it for scripts too): the cell / script code ends
  // up in a wrapper object, where opaque types are allowed.
  // Our callers pass `outermost = true` when parsing a whole cell / script block, whose
  // top-level language imports (`import language.experimental.captureChecking`, source version
  // imports, ...) Preprocessor hoists to the top level of the code it generates. Blocks nested
  // in the cell go through the `outermost = false` default.
  override def blockStatSeq(outermost: Boolean = false): List[untpd.Tree] = checkNoEscapingPlaceholders {
    val stats = new mutable.ListBuffer[untpd.Tree]
    while
      var empty = false
      if (in.token == Tokens.IMPORT)
        stats ++= importClause(outermost)
      else if (outermost && in.token == Tokens.EXPORT)
        // Unlike dotty's REPL, we accept export clauses at the outermost level, as they end
        // up in the wrapper object of the cell / script, where they are allowed
        stats ++= exportClause()
      else if (isExprIntro)
        stats += expr(Location.InBlock)
      else if in.token == Tokens.IMPLICIT && !in.inModifierPosition() then
        stats += closure(
          in.offset,
          Location.InBlock,
          modifiers(scala.collection.immutable.BitSet(Tokens.IMPLICIT))
        )
      else if isIdent(nme.extension) && followingIsExtension() then
        stats += extension()
      else if isDefIntro(
        oursLocalModifierTokens,
        excludedSoftModifiers = if outermost then Set.empty else Set(nme.`opaque`)
      ) then
        stats +++= localDef(in.offset)
      else
        empty = true
      statSepOrEnd(stats, noPrevStat = empty, altEnd = Tokens.CASE)
    do ()
    stats.toList
  }
}
