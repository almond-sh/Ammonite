package ammonite.compiler.internal

import java.nio.file.FileSystem

import scala.tools.nsc.util.ClassPath

/**
 * Creates the class path of the JDK modules, read from a `jrt:/` file system.
 *
 * scalac's `JrtClassPath` takes a flag telling whether to close that file system along with it
 * since 2.13.15.
 */
trait JrtClassPathMaker {

  /** The class path of the JDK modules in `fs`, which the class path won't close. */
  def jrtClassPath(fs: FileSystem): ClassPath
}
