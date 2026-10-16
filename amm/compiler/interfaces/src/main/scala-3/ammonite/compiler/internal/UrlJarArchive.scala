package ammonite.compiler.internal

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import java.net.URL
import java.util.concurrent.ConcurrentHashMap
import java.util.zip.{ZipEntry, ZipFile, ZipInputStream}

import scala.collection.mutable

import ammonite.util.Classpath
import dotty.tools.io.{AbstractFile, NoAbstractFile}

/**
 * The content of a JAR that Ammonite can only reach through a URL, as a tree of dotty
 * `AbstractFile`s, that the compiler class path of [[UrlJarClassPathMaker]] reads.
 *
 * dotty only reads JARs from the file system (or from `ManifestResources`, that we can't
 * extend). This is what lets it read those reached through URLs, like the JARs that
 * coursier standalone launchers embed.
 *
 * When the URL points to the root of a JAR that a `JarURLConnection` can open, like those
 * of the JARs embedded in coursier standalone launchers, the entries are listed from that
 * JAR and only read when the compiler asks for them. Otherwise, the whole JAR is read and
 * decompressed upfront.
 *
 * Compiled against the oldest dotty we support, and run with the later ones, so this only
 * relies on the parts of `AbstractFile` that didn't change in between.
 */
object UrlJarArchive {

  // Keyed by the URL string, as URL#equals and URL#hashCode can resolve host names
  private val cache = new ConcurrentHashMap[String, Dir]

  /**
   * The root directory of the JAR at `url`, shared between the compiler instances Ammonite
   * creates, so that re-creating a compiler (upon adding dependencies, for example) doesn't
   * list or read the JARs of the class path again.
   */
  def root(url: URL): Dir = {
    val key = url.toExternalForm
    val existing = cache.get(key)
    if (existing == null) {
      val root = read(url)
      val previous = cache.putIfAbsent(key, root)
      if (previous == null) root else previous
    } else
      existing
  }

  private def read(url: URL): Dir = {
    val root = new Dir(url.toExternalForm, "", null)
    Classpath.openJar(url) match {
      case Some(zipFile) =>
        val it = zipFile.entries()
        while (it.hasMoreElements) {
          val entry = it.nextElement()
          if (!entry.isDirectory)
            root.add(entry, () => zipFile.getInputStream(entry), entry.getSize)
        }
      case None =>
        val zis = new ZipInputStream(url.openStream())
        try {
          var entry = zis.getNextEntry
          while (entry != null) {
            if (!entry.isDirectory) {
              // the entries of a ZipInputStream don't know their size, unlike those of a ZipFile
              val content = readFully(zis)
              root.add(entry, () => new ByteArrayInputStream(content), content.length)
            }
            entry = zis.getNextEntry
          }
        } finally zis.close()
    }
    root
  }

  private def readFully(is: InputStream): Array[Byte] = {
    val baos = new ByteArrayOutputStream
    val buf = new Array[Byte](16 * 1024)
    var read = is.read(buf)
    while (read >= 0) {
      baos.write(buf, 0, read)
      read = is.read(buf)
    }
    baos.toByteArray
  }

  /** A directory of the JAR, `path` being its path in the JAR, like `scala/collection/` */
  final class Dir(val name: String, val path: String, parent: Dir | Null) extends AbstractFile {
    private val dirs = mutable.LinkedHashMap.empty[String, Dir]
    private val files = mutable.LinkedHashMap.empty[String, File]

    private[UrlJarArchive] def add(entry: ZipEntry, input: () => InputStream, size: Long): Unit = {
      val elems = entry.getName.split('/').filter(_.nonEmpty)
      if (elems.nonEmpty) {
        val dir = elems.init.foldLeft(this) { (dir, elem) =>
          dir.dirs.getOrElseUpdate(elem, new Dir(elem, dir.path + elem + "/", dir))
        }
        val fileName = elems.last
        dir.files(fileName) =
          new File(fileName, entry.getName, dir, input, size, entry.getTime)
      }
    }

    def isDirectory: Boolean = true
    def iterator(): Iterator[AbstractFile] = dirs.valuesIterator ++ files.valuesIterator
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

    override def toString: String = s"Dir($name, $path)"
  }

  /** A file of the JAR, `path` being its path in the JAR, like `scala/Predef.class` */
  final class File(
      val name: String,
      val path: String,
      parent: Dir,
      read: () => InputStream,
      size: Long,
      time: Long
  ) extends AbstractFile {
    def isDirectory: Boolean = false
    def iterator(): Iterator[AbstractFile] = Iterator.empty
    def lookupName(name: String, directory: Boolean): AbstractFile = null
    def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = unsupported()

    // dotty looks for the TASTy file of a class file in the container of the latter
    def container: AbstractFile = parent
    def absolute: AbstractFile = this
    def jpath: java.nio.file.Path = null
    def lastModified: Long = time
    def input: InputStream = read()
    override def sizeOption: Option[Int] = if (size >= 0) Some(size.toInt) else None
    def output: OutputStream = unsupported()
    def create(): Unit = unsupported()
    def delete(): Unit = unsupported()

    override def toString: String = s"File($path)"
  }
}
