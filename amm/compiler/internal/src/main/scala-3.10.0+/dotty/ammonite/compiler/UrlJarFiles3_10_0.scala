package dotty.ammonite.compiler

import java.io.{InputStream, OutputStream}
import java.net.URL

import ammonite.compiler.internal.UrlJarArchive
import dotty.tools.io.AbstractFile

/** The [[UrlJarArchive]] entries, as the `AbstractFile`s of the compilers of 3.10.0 and later. */
object UrlJarFiles3_10_0 {

  /** The root directory of `dir`, as an `AbstractFile` */
  def root(dir: UrlJarArchive.Dir): AbstractFile =
    new Dir(dir, None)

  private final class Dir(dir: UrlJarArchive.Dir, parent: Option[Dir]) extends AbstractFile {
    private lazy val dirs: Map[String, Dir] =
      dir.dirs.iterator.map { case (name, sub) => name -> new Dir(sub, Some(this)) }.toMap
    private lazy val files: Map[String, File] =
      dir.files.iterator.map { case (name, file) => name -> new File(file, this) }.toMap

    def name: String = dir.name
    def path: String = dir.path
    def isDirectory: Boolean = true
    def iterator: Iterator[AbstractFile] =
      dir.dirs.keysIterator.map(dirs) ++ dir.files.keysIterator.map(files)
    def lookupName(name: String, directory: Boolean): AbstractFile | Null =
      if (directory) dirs.getOrElse(name, null)
      else files.getOrElse(name, null)

    override def container: Option[AbstractFile] = parent
    def jpath: java.nio.file.Path | Null = null
    def toURL: Option[URL] = None
    def lastModified: Long = 0L
    def input: InputStream = unsupported()
    def output: OutputStream = unsupported()

    override def toString: String = dir.toString
  }

  private final class File(file: UrlJarArchive.File, parent: Dir) extends AbstractFile {
    def name: String = file.name
    def path: String = file.path
    def isDirectory: Boolean = false
    def iterator: Iterator[AbstractFile] = Iterator.empty
    def lookupName(name: String, directory: Boolean): AbstractFile | Null = null

    // dotty looks for the TASTy file of a class file in the container of the latter
    override def container: Option[AbstractFile] = Some(parent)
    def jpath: java.nio.file.Path | Null = null
    def toURL: Option[URL] = None
    def lastModified: Long = file.time
    def input: InputStream = file.input()
    def output: OutputStream = unsupported()

    override def toString: String = file.toString
  }
}
