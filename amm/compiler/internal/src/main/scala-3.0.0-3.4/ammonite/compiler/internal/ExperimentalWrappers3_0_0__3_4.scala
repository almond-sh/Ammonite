package ammonite.compiler.internal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.util.Spans.Span

/**
 * The [[ExperimentalWrappers]] for the compilers of 3.0.0 to 3.4.x, which only accept
 * experimental language imports at the top level of a unit whose top-level definitions are
 * all `@experimental`.
 */
class ExperimentalWrappers3_0_0__3_4 extends ExperimentalWrappers {

  // The experimental features these compilers accept anywhere
  private val allowedAnywhere = Set("macros", "erasedDefinitions")

  private def hasExperimentalImport(stats: List[untpd.Tree])(using Context): Boolean =
    stats.exists {
      case untpd.Import(expr, selectors) =>
        untpd.languageImport(expr).contains(nme.experimental) &&
          !selectors.forall(sel => allowedAnywhere(sel.name.toString))
      case _ => false
    }

  private def experimentalAnnotation(span: Span)(using Context): untpd.Tree = {
    def at[T <: untpd.Tree](tree: T): T = tree.withSpan(span).asInstanceOf[T]
    val scalaPackage = at(untpd.Select(at(untpd.Ident(nme.ROOTPKG)), nme.scala))
    val annotationPackage = at(untpd.Select(scalaPackage, termName("annotation")))
    at(untpd.New(at(untpd.Select(annotationPackage, typeName("experimental"))), Nil))
  }

  private def markExperimental0(stat: untpd.Tree)(using Context): untpd.Tree =
    stat match {
      case mdef: untpd.MemberDef =>
        mdef.withMods(mdef.mods.withAddedAnnotation(experimentalAnnotation(mdef.span.startPos)))
      case other => other
    }

  private def isLanguageImport(stat: untpd.Tree)(using Context): Boolean =
    stat match {
      case i: untpd.Import => untpd.languageImport(i.expr).isDefined
      case _ => false
    }

  private def withImports(stat: untpd.Tree, imports: List[untpd.Tree])(using Context): untpd.Tree =
    stat match {
      case mdef @ untpd.ModuleDef(name, impl) =>
        untpd.cpy.ModuleDef(mdef)(name, untpd.cpy.Template(impl)(body = imports ::: impl.body))
      case tdef @ untpd.TypeDef(name, impl: untpd.Template) =>
        untpd.cpy.TypeDef(tdef)(name, untpd.cpy.Template(impl)(body = imports ::: impl.body))
      case other => other
    }

  def markExperimental(tree: untpd.Tree)(using Context): untpd.Tree =
    tree match {
      case pkg @ untpd.PackageDef(pid, stats) =>
        val stats0 = stats.map(markExperimental)
        val stats1 =
          if (hasExperimentalImport(stats0)) {
            // Imports at the package level aren't in an experimental scope, and can't refer to
            // @experimental definitions, like the wrappers of the previous commands now are.
            // So we move them into the wrapper, the last template definition of the package
            // (AmmonitePhase tells them apart from those of the user code by their position).
            val (imports, others) = stats0.partition {
              case i: untpd.Import => !isLanguageImport(i)
              case _ => false
            }
            val wrapperIdx = others.lastIndexWhere {
              case _: untpd.ModuleDef => true
              case untpd.TypeDef(_, _: untpd.Template) => true
              case _ => false
            }
            val others0 =
              if (imports.isEmpty || wrapperIdx < 0) stats0
              else others.updated(wrapperIdx, withImports(others(wrapperIdx), imports))
            others0.map(markExperimental0)
          }
          else stats0
        untpd.cpy.PackageDef(pkg)(pid, stats1)
      case other => other
    }
}
