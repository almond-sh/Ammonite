package ammonite.compiler.internal

import java.nio.file.FileSystem

import scala.tools.nsc.classpath.JrtClassPath
import scala.tools.nsc.util.ClassPath

/** The [[JrtClassPathMaker]] for the scalac of 2.13.15 and later. */
class JrtClassPathMaker2_13_15 extends JrtClassPathMaker {
  def jrtClassPath(fs: FileSystem): ClassPath = new JrtClassPath(fs, closeFS = false)
}
