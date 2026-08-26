package ammonite.compiler.internal

import scala.tools.nsc
import scala.tools.nsc.classpath.AggregateClassPath
import scala.tools.nsc.interactive.{Global => InteractiveGlobal}
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.{Global, Settings}

/**
 * The [[GlobalMaker]] for the scalac of 2.12.13 and later.
 *
 * The presentation compiler gets the analyzer scalac gives it: unlike the batch compiler, it
 * does not need Ammonite's macro class loader, and staying off `InteractiveAnalyzer` keeps a
 * good hundred kilobytes of mixin forwarders out of the JAR.
 */
class GlobalMaker2_12_13 extends GlobalMaker {

  def global(
      settings: Settings,
      reporter: Reporter,
      jcp: AggregateClassPath,
      evalClassLoader: ClassLoader,
      createPlugins: Global => List[Plugin]
  ): Global =
    new nsc.Global(settings, reporter) { g =>
      override lazy val plugins = createPlugins(g)

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
      override def classPath = jcp

      override lazy val platform: ThisPlatform = new GlobalPlatform {
        override val global = g
        override val settings = g.settings
        override val classPath = jcp
      }

    }

  def importInfo(g: Global)(tree: g.Import): g.analyzer.ImportInfo =
    new g.analyzer.ImportInfo(tree, 0)

  def resetReporter(g: Global): Unit =
    g.reporter.reset()
}
