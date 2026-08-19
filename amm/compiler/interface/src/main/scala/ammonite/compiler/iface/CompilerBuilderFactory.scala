package ammonite.compiler.iface

import java.nio.file.Path
import java.util.ServiceLoader

/**
 * Entry point to a concrete compiler implementation.
 *
 * The `ammonite-compiler` module is published for each supported full Scala version
 * (`ammonite-compiler_2.13.16`, `ammonite-compiler_3.3.8`, …), while the modules using it
 * are published for binary Scala versions only (`ammonite-repl_2.13`, `ammonite_3`, …).
 * Those modules therefore can't depend on `ammonite-compiler` directly, and go through this
 * factory instead, which is looked up on the class path with [[java.util.ServiceLoader]].
 *
 * Users are expected to add a dependency on the `ammonite-compiler` variant matching the
 * exact Scala version they run with.
 */
abstract class CompilerBuilderFactory {

  /** The Scala version `ammonite-compiler` was built for. */
  def scalaVersion: String

  def compilerBuilder(outputDir: Option[Path] = None): CompilerBuilder

  def parser: Parser
}

object CompilerBuilderFactory {

  /**
   * Loads the [[CompilerBuilderFactory]] provided by `ammonite-compiler`.
   *
   * Tries the context class loader first, then the one this class was loaded from.
   *
   * @throws NoCompilerException if no `ammonite-compiler` is on the class path
   */
  def load(): CompilerBuilderFactory =
    Option(Thread.currentThread().getContextClassLoader)
      .flatMap(load0)
      .orElse(load0(classOf[CompilerBuilderFactory].getClassLoader))
      .getOrElse(throw new NoCompilerException)

  /** Same as [[load()]], but looking into `loader` only. */
  def load(loader: ClassLoader): CompilerBuilderFactory =
    load0(loader).getOrElse(throw new NoCompilerException)

  private def load0(loader: ClassLoader): Option[CompilerBuilderFactory] = {
    val it = ServiceLoader.load(classOf[CompilerBuilderFactory], loader).iterator()
    if (it.hasNext) Some(it.next())
    else None
  }

  final class NoCompilerException extends Exception(
        "No Ammonite compiler found on the class path. Add a dependency on the " +
          "ammonite-compiler module matching the exact Scala version you run with, like " +
          "\"sh.almond.ammonite:ammonite-compiler_<scala-version>:<ammonite-version>\"."
      )
}
