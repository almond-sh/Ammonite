package ammonite.compiler.internal

import java.nio.file.FileSystem

import scala.tools.nsc.classpath.JrtClassPath
import scala.tools.nsc.util.ClassPath

/** The [[JrtClassPathMaker]] for the scalac of 2.12.x to 2.13.14. */
class JrtClassPathMaker2_12_0__2_13_14 extends JrtClassPathMaker {
  def jrtClassPath(fs: FileSystem): ClassPath = new JrtClassPath(fs)
}
