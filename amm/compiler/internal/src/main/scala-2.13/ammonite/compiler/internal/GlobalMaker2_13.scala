package ammonite.compiler.internal

import scala.tools.nsc
import scala.tools.nsc.classpath.AggregateClassPath
import scala.tools.nsc.interactive.{Global => InteractiveGlobal}
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.{Global, Settings}

/**
 * The [[GlobalMaker]] for the scalac of 2.13.
 *
 * Neither compiler subclasses an analyzer of its own: scalac already mixes
 * `MacroAnnotationNamers` into `Global.analyzer` under `-Ymacro-annotations`, and the macro
 * class loader it hands macro expansion is the one `Global.findMacroClassLoader` returns,
 * which is overridden below - the same hook scalac's own REPL uses. Staying off the analyzer
 * matters: it is a trait, and it grows fields in nearly every 2.13 patch release, so a
 * subclass of it only ever links against the exact scalac it was compiled with.
 */
class GlobalMaker2_13 extends GlobalMaker {

  def global(
      settings: Settings,
      reporter: Reporter,
      jcp: AggregateClassPath,
      evalClassLoader: ClassLoader,
      createPlugins: Global => List[Plugin]
  ): Global =
    new nsc.Global(settings, reporter) { g =>
      override lazy val plugins = createPlugins(g)

      // Actually jcp, avoiding a path-dependent type issue here
      override def classPath = jcp
      override def findMacroClassLoader() = evalClassLoader

      override lazy val platform: ThisPlatform = new GlobalPlatform {
        override val global = g
        override val settings = g.settings
        override val classPath = jcp
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
    new g.analyzer.ImportInfo(tree, 0, false)

  def resetReporter(g: Global): Unit =
    g.reporter.reset()
}
