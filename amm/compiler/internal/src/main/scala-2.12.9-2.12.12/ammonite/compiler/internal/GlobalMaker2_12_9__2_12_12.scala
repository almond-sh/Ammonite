package ammonite.compiler.internal

import scala.tools.nsc
import scala.tools.nsc.classpath.AggregateClassPath
import scala.tools.nsc.interactive.{InteractiveAnalyzer, Global => InteractiveGlobal}
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.{Global, Settings}

/**
 * The [[GlobalMaker]] for the scalac of 2.12.9 to 2.12.12.
 *
 * Its analyzers look the macro class loader up themselves rather than being handed it by
 * the compiler, as 2.13 does.
 *
 * Scala 2.12 has no `Global.findMacroClassLoader` to override, so this has to subclass
 * `Analyzer`, and `Analyzer` is a trait: a subclass of it only links against a scalac whose
 * typechecker traits declare no field it does not already set. That is what pins this to
 * 2.12.9 to 2.12.12 - 2.12.9 added fields to `Contexts`, `Implicits` and `Macros`.
 */
class GlobalMaker2_12_9__2_12_12 extends GlobalMaker {

  def global(
      settings: Settings,
      reporter: Reporter,
      jcp: AggregateClassPath,
      evalClassLoader: ClassLoader,
      createPlugins: Global => List[Plugin]
  ): Global =
    new nsc.Global(settings, reporter) { g =>
      override lazy val plugins = createPlugins(g)

      // Actually jcp, avoiding a path-dependent type issue in 2.10 here
      override def classPath = jcp

      override lazy val platform: ThisPlatform = new GlobalPlatform {
        override val global = g
        override val settings = g.settings
        override val classPath = jcp
      }

      override lazy val analyzer = new { val global: g.type = g } with Analyzer {
        override def findMacroClassLoader() = evalClassLoader
      }
    }

  def interactiveGlobal(
      settings: Settings,
      reporter: Reporter,
      jcp: AggregateClassPath,
      evalClassLoader: ClassLoader
  ): InteractiveGlobal =
    new nsc.interactive.Global(settings, reporter) { g =>
      // Actually jcp, avoiding a path-dependent type issue in 2.10 here
      override def classPath = jcp

      override lazy val platform: ThisPlatform = new GlobalPlatform {
        override val global = g
        override val settings = g.settings
        override val classPath = jcp
      }

      override lazy val analyzer = new { val global: g.type = g } with InteractiveAnalyzer {
        override def findMacroClassLoader() = evalClassLoader
      }
    }

  def importInfo(g: Global)(tree: g.Import): g.analyzer.ImportInfo =
    new g.analyzer.ImportInfo(tree, 0)

  def resetReporter(g: Global): Unit =
    g.reporter.reset()
}
