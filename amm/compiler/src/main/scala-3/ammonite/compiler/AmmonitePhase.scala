package ammonite.compiler

import ammonite.compiler.internal.CompilerInternals
import ammonite.util.{ImportData, Imports, Name => AmmName, Printer, Util}

import dotty.tools.dotc
import dotty.tools.dotc.core.StdNames.nme
import dotc.ast.Trees._
import dotc.ast.{tpd, untpd}
import dotc.core.Flags
import dotc.core.Contexts._
import dotc.core.Names.Name
import dotc.core.Phases.Phase
import dotc.core.Symbols.{NoSymbol, Symbol, defn, newSymbol}
import dotc.core.Types.{
  ExprType,
  MethodType,
  PolyType,
  TermRef,
  Type,
  TypeTraverser,
  WildcardType
}

import scala.collection.mutable

class AmmonitePhase(
  userCodeNestingLevel: => Int,
  needsUsedEarlierDefinitions: => Boolean,
  userCodeOffset: => Int
) extends Phase:
  import tpd._

  def phaseName: String = "ammonite"

  private var myImports = new mutable.ListBuffer[(Boolean, String, String, Seq[AmmName])]
  private var usedEarlierDefinitions0 = new mutable.ListBuffer[String]

  def importData: Seq[ImportData] =
    val grouped = myImports
      .toList
      .distinct
      .groupBy { case (a, b, c, d) => (b, c, d) }
      .mapValues(_.map(_._1))

    val open = for {
      ((fromName, toName, importString), items) <- grouped
      if !CompilerUtil.ignoredNames(fromName)
    } yield {
      val importType = items match{
        case Seq(true) => ImportData.Type
        case Seq(false) => ImportData.Term
        case Seq(_, _) => ImportData.TermType
      }

      ImportData(AmmName(fromName), AmmName(toName), importString, importType)
    }

    open
      .toVector
      // Source version imports only apply to the compilation unit they're in
      .filter(!LanguageImports.isSourceVersionImport(_))
      .sortBy(x => Util.encodeScalaSourcePath(x.prefix))

  def usedEarlierDefinitions: Seq[String] =
    usedEarlierDefinitions0.toList.distinct

  private def saneSym(name: Name, sym: Symbol)(using Context): Boolean =
    !name.decode.toString.contains('$') &&
    sym.exists &&
    // !sym.is(Flags.Synthetic) &&
    !scala.util.Try(sym.is(Flags.Private)).toOption.getOrElse(true) &&
    !scala.util.Try(sym.is(Flags.Protected)).toOption.getOrElse(true) &&
    // sym.is(Flags.Public) &&
    !CompilerUtil.ignoredSyms(sym.toString) &&
    !CompilerUtil.ignoredNames(name.decode.toString)

  private def saneSym(sym: Symbol)(using Context): Boolean =
    saneSym(sym.name, sym)

  private def processTree(t: tpd.Tree)(using Context): Unit = {
    val sym = t.symbol
    val name = t match {
      case t: tpd.ValDef => t.name
      case _ => sym.name
    }
    if (saneSym(name, sym)) {
      val name = sym.name.decode.toString
      myImports.addOne((sym.isType, name, name, Nil))
    }
  }

  private def processImport(i: tpd.Import)(using Context): Unit = {
    val expr = i.expr
    val selectors = i.selectors

    // Most of that logic was adapted from AmmonitePlugin, the Scala 2 counterpart
    // of this file.

    val prefix =
      val (_ :: nameListTail, symbolHead :: _) = {
        def rec(expr: tpd.Tree): List[(Name, Symbol)] = {
          expr match {
            case s @ tpd.Select(lhs, _) => (s.symbol.name -> s.symbol) :: rec(lhs)
            case i @ tpd.Ident(name) => List(name -> i.symbol)
            case t @ tpd.This(pkg) => List(pkg.name -> t.symbol)
          }
        }
        rec(expr).reverse.unzip
      }

      val headFullPath =
        // `import _root_.a.b`
        if (symbolHead == defn.RootPackage || symbolHead == defn.RootClass) Array.empty[String]
        else
          symbolHead.fullName.decode.toString.split('.')
            .map(n => if (n.endsWith("$")) n.stripSuffix("$") else n) // meh
      // prefix package imports with `_root_` to try and stop random
      // variables from interfering with them. If someone defines a value
      // called `_root_`, this will still break, but that's their problem
      val rootPrefix = if(symbolHead.denot.is(Flags.Package)) Seq("_root_") else Nil
      val tailPath = nameListTail.map(_.decode.toString)

      (rootPrefix ++ headFullPath ++ tailPath).map(AmmName(_))

    def isMask(sel: untpd.ImportSelector) = sel.name != nme.WILDCARD && sel.rename == nme.WILDCARD

    val renameMap =

      /**
        * A map of each name importable from `expr`, to a `Seq[Boolean]`
        * containing a `true` if there's a type-symbol you can import, `false`
        * if there's a non-type symbol and both if there are both type and
        * non-type symbols that are importable for that name
        */
      val importableIsTypes =
        expr.tpe
            .allMembers
            .map(_.symbol)
            .filter(saneSym(_))
            .groupBy(_.name.decode.toString)
            .mapValues(_.map(_.isType).toVector)

      val renamings = for{
        t @ untpd.ImportSelector(name, renameTree, _) <- selectors
        if !isMask(t)
        // getOrElse just in case...
        isType <- importableIsTypes.getOrElse(name.name.decode.toString, Nil)
        rename <- Option(renameTree).collect{ case Ident(r) => r }
      } yield ((isType, rename.decode.toString), name.name.decode.toString)

      renamings.toMap


    def isUnimportableUnlessRenamed(sym: Symbol): Boolean =
      sym eq NoSymbol

    def namedSelectorMatches(sel: untpd.ImportSelector, sym: Symbol): Boolean =
      !sel.isWildcard && {
        val from = sel.imported
        from.name == (if (from.isTerm) sym.name.toTermName else sym.name.toTypeName)
      }

    /** The type of the bound of a `given T` selector, if any */
    def selectorBound(sel: untpd.ImportSelector): Type =
      sel.bound match {
        case untpd.TypedSplice(t) => t.tpe
        case t => t.typeOpt
      }

    // Approximates the type a given provides, so that it can be checked against
    // the bound of a `given T` selector (type parameters are replaced by wildcards,
    // using clauses are skipped)
    def providedType(tpe: Type): Type =
      tpe match {
        case pt: PolyType => providedType(pt.instantiate(pt.paramNames.map(_ => WildcardType)))
        case mt: MethodType => providedType(mt.resultType)
        case et: ExprType => providedType(et.resultType)
        case _ => tpe
      }

    // Follows Scala 3 semantics: `*` (or `_`) imports everything but givens,
    // `given` imports only givens (and old-style implicits), `given T` only
    // those conforming to `T`
    def wildcardImports(sel: untpd.ImportSelector, sym: Symbol): Boolean =
      if (sel.isGiven)
        sym.isOneOf(Flags.GivenOrImplicit) && {
          val bound = selectorBound(sel)
          !bound.exists ||
            scala.util.Try(providedType(expr.tpe.memberInfo(sym)) <:< bound).getOrElse(true)
        }
      else
        !sym.is(Flags.Given)

    def transformImport(selectors: List[untpd.ImportSelector], sym: Symbol): List[Symbol] =
      selectors.find(namedSelectorMatches(_, sym)) match {
        case Some(sel) =>
          if (isMask(sel)) Nil
          else List(
            newSymbol(sym.owner, sel.rename, sym.flags, sym.info, sym.privateWithin, sym.coord)
          )
        case None =>
          if (isUnimportableUnlessRenamed(sym)) Nil
          else if (selectors.exists(sel => sel.isWildcard && wildcardImports(sel, sym))) List(sym)
          else Nil
      }

    val symNames =
      for {
        sym <- expr.tpe.allMembers.map(_.symbol).flatMap(transformImport(selectors, _))
        if saneSym(sym)
      } yield (sym.isType, sym.name.decode.toString)

    val syms = for {
      // For some reason `info.allImportedSymbols` does not show imported
      // type aliases when they are imported directly e.g.
      //
      // import scala.reflect.macros.Context
      //
      // As opposed to via import scala.reflect.macros._.
      // Thus we need to combine allImportedSymbols with the renameMap
      (isType, sym) <- (symNames.toList ++ renameMap.keys).distinct
    } yield (isType, renameMap.getOrElse((isType, sym), sym), sym, prefix)

    myImports ++= syms
  }

  private def updateUsedEarlierDefinitions(
    wrapperSym: Symbol,
    stats: List[tpd.Tree]
  )(using Context): Unit = {
    /*
     * We list the variables from the first wrapper
     * used from the user code.
     *
     * E.g. if, after wrapping, the code looks like
     * ```
     *   class cmd2 {
     *
     *     val cmd0 = ???
     *     val cmd1 = ???
     *
     *     import cmd0.{
     *       n
     *     }
     *
     *     class Helper {
     *       // user-typed code
     *       val n0 = n + 1
     *     }
     *   }
     * ```
     * this would process the tree of `val n0 = n + 1`, find `n` as a tree like
     * `cmd2.this.cmd0.n`, and put `cmd0` in `uses`.
     */

    val typeTraverser: TypeTraverser = new TypeTraverser {
      def traverse(tpe: Type) = tpe match {
        case tr: TermRef if tr.prefix.typeSymbol == wrapperSym =>
          // no name to record if the reference points at neither a name nor a symbol
          CompilerInternals.termRefNames.designatorName(tr).foreach(usedEarlierDefinitions0 += _)
        case _ =>
          traverseChildren(tpe)
      }
    }

    val traverser: TreeTraverser = new TreeTraverser {
      def traverse(tree: Tree)(using Context) = tree match {
        case tpd.Select(node, name) if node.symbol == wrapperSym =>
          usedEarlierDefinitions0 += name.decode.toString
        case tt @ tpd.TypeTree() =>
          typeTraverser.traverse(tt.tpe)
        case _ =>
          traverseChildren(tree)
      }
    }

    for (tree <- stats)
      traverser.traverse(tree)
  }

  private def isLanguageImport(i: tpd.Import)(using Context): Boolean =
    val sym = i.expr.symbol
    sym.exists && {
      // like scala.language$.experimental
      val name = sym.fullName.decode.toString.split('.').map(_.stripSuffix("$")).toSeq
      name == Seq("scala", "language") || name == Seq("scala", "language", "experimental")
    }

  private def unpkg(tree: tpd.Tree): List[tpd.Tree] =
    tree match {
      case PackageDef(_, elems) => elems.flatMap(unpkg)
      case _ => List(tree)
    }

  def run(using Context): Unit =
    val elems = unpkg(ctx.compilationUnit.tpdTree)
    def mainStats(trees: List[tpd.Tree]): List[tpd.Tree] =
      trees
        .reverseIterator
        .collectFirst {
          case TypeDef(name, rhs0: Template) => rhs0.body
        }
        .getOrElse(Nil)

    val rootStats = mainStats(elems)
    val stats = (1 until userCodeNestingLevel)
      .foldLeft(rootStats)((trees, _) => mainStats(trees))

    if (needsUsedEarlierDefinitions) {
      val wrapperSym = elems.last.symbol
      updateUsedEarlierDefinitions(wrapperSym, stats)
    }

    // The language imports Preprocessor hoisted at the top level of the code, which are those
    // of the user code, and those of the previous commands, that we keep passing along
    elems.foreach {
      case i: Import if isLanguageImport(i) => processImport(i)
      case _ =>
    }

    // Imports from the generated code that ended up next to the user code (see
    // ExperimentalWrappers) are those of the previous commands, not new ones
    def isUserCode(t: tpd.Tree) = !t.span.exists || t.span.start >= userCodeOffset

    stats.foreach {
      case i: Import if isUserCode(i) => processImport(i)
      case _: Import =>
      case t: tpd.DefDef => processTree(t)
      case t: tpd.ValDef => processTree(t)
      case t: tpd.TypeDef => processTree(t)
      case _ =>
    }
