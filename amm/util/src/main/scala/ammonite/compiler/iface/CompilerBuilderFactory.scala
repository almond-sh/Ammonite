package ammonite.compiler.iface

import java.nio.file.Path
import java.util.ServiceLoader

/**
 * Entry point to a concrete compiler implementation.
 *
 * `ammonite-compiler` drags `scala-compiler` in, which the modules that only ever run cached
 * scripts have no use for, so none of them depends on it. They go through this factory
 * instead, which is looked up on the class path with [[java.util.ServiceLoader]].
 *
 * Users are expected to add a dependency on `ammonite-compiler` themselves.
 */
abstract class CompilerBuilderFactory {

  /** The Scala version `ammonite-compiler` is running with. */
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
          "ammonite-compiler module, like " +
          "\"sh.almond.ammonite::ammonite-compiler:<ammonite-version>\"."
      )
}
