package ammonite.compiler.internal

import java.io.{InputStream, OutputStream}

import dotty.tools.io.{AbstractFile, NoAbstractFile}

/**
 * The [[UrlJarArchive]] entries, as the `AbstractFile`s of the compilers up to 3.9.x.
 *
 * 3.10.0 reworked `AbstractFile` (`container` hands back an `Option`, `NoAbstractFile` is
 * gone, ...): these can't even be loaded with it, and have a counterpart of their own in its
 * `amm.compiler.internal` sources.
 */
object UrlJarFiles {

  /** The root directory of `dir`, as an `AbstractFile` */
  def root(dir: UrlJarArchive.Dir): AbstractFile =
    new Dir(dir, null)

  private final class Dir(dir: UrlJarArchive.Dir, parent: Dir | Null) extends AbstractFile {
    private lazy val dirs: Map[String, Dir] =
      dir.dirs.iterator.map { case (name, sub) => name -> new Dir(sub, this) }.toMap
    private lazy val files: Map[String, File] =
      dir.files.iterator.map { case (name, file) => name -> new File(file, this) }.toMap

    def name: String = dir.name
    def path: String = dir.path
    def isDirectory: Boolean = true
    def iterator(): Iterator[AbstractFile] =
      dir.dirs.keysIterator.map(dirs) ++ dir.files.keysIterator.map(files)
    def lookupName(name: String, directory: Boolean): AbstractFile =
      if (directory) dirs.getOrElse(name, null)
      else files.getOrElse(name, null)
    def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = unsupported()

    def container: AbstractFile = if (parent == null) NoAbstractFile else parent
    def absolute: AbstractFile = this
    def jpath: java.nio.file.Path = null
    def lastModified: Long = 0L
    def input: InputStream = unsupported()
    def output: OutputStream = unsupported()
    def create(): Unit = unsupported()
    def delete(): Unit = unsupported()

    override def toString: String = dir.toString
  }

  private final class File(file: UrlJarArchive.File, parent: Dir) extends AbstractFile {
    def name: String = file.name
    def path: String = file.path
    def isDirectory: Boolean = false
    def iterator(): Iterator[AbstractFile] = Iterator.empty
    def lookupName(name: String, directory: Boolean): AbstractFile = null
    def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = unsupported()

    // dotty looks for the TASTy file of a class file in the container of the latter
    def container: AbstractFile = parent
    def absolute: AbstractFile = this
    def jpath: java.nio.file.Path = null
    def lastModified: Long = file.time
    def input: InputStream = file.input()
    override def sizeOption: Option[Int] = if (file.size >= 0) Some(file.size.toInt) else None
    def output: OutputStream = unsupported()
    def create(): Unit = unsupported()
    def delete(): Unit = unsupported()

    override def toString: String = file.toString
  }
}
