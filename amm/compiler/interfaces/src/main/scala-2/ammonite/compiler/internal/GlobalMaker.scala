package ammonite.compiler.internal

import scala.tools.nsc.classpath.AggregateClassPath
import scala.tools.nsc.interactive.{Global => InteractiveGlobal}
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.{Global, Settings}

/**
 * Creates the compilers Ammonite compiles and completes with, along with the odd bit of
 * scalac state that has to be built alongside them.
 *
 * How the macro class loader reaches the analyzer changed in 2.12.13, which also narrowed
 * the return type of `Global.reporter` - source-compatible, but not binary-compatible. 2.13
 * wires macro annotations into the analyzer, and `ImportInfo` takes a third parameter there.
 */
trait GlobalMaker {

  /** The compiler Ammonite compiles with, class path, plugins and all. */
  def global(
      settings: Settings,
      reporter: Reporter,
      jcp: AggregateClassPath,
      evalClassLoader: ClassLoader,
      createPlugins: Global => List[Plugin]
  ): Global

  /** The presentation compiler Ammonite completes with. */
  def interactiveGlobal(
      settings: Settings,
      reporter: Reporter,
      jcp: AggregateClassPath,
      evalClassLoader: ClassLoader
  ): InteractiveGlobal

  /** The import `tree` stands for, as the typer's context tracks it. */
  def importInfo(g: Global)(tree: g.Import): g.analyzer.ImportInfo

  /** Clears the errors `g`'s own reporter has accumulated. */
  def resetReporter(g: Global): Unit
}
