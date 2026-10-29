package ammonite.compiler

import java.nio.file.{Files, Path}

object CompilerUtil {

  /**
   * Writes `bytes` to the file at `segments` under `dir`, creating its parent directories
   * and overwriting it if needed.
   */
  def writeOver(dir: Path, segments: Seq[String], bytes: Array[Byte]): Unit = {
    val file = segments.foldLeft(dir)(_.resolve(_))
    Files.createDirectories(file.getParent)
    Files.write(file, bytes)
  }

  /** A new temporary directory, deleted on exit if it is empty by then. */
  def tempDir(): Path = {
    val dir = Files.createTempDirectory("ammonite")
    dir.toFile.deleteOnExit()
    dir
  }

  val ignoredSyms = Set(
    "package class-use",
    "object package-info",
    "class package-info"
  )
  val ignoredNames = Set(
    // Probably synthetic
    "<init>",
    "<clinit>",
    "$main",
    // Don't care about this
    "toString",
    "equals",
    "wait",
    "notify",
    "notifyAll",
    "synchronized",
    "hashCode",
    "getClass",
    "eq",
    "ne",
    "##",
    "==",
    "!=",
    "isInstanceOf",
    "asInstanceOf",
    // Behaves weird in 2.10.x, better to just ignore.
    "_"
  )

}
