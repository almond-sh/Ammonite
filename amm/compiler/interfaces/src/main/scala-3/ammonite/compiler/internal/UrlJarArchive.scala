package ammonite.compiler.internal

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}
import java.net.URL
import java.util.concurrent.ConcurrentHashMap
import java.util.zip.{ZipEntry, ZipFile, ZipInputStream}

import scala.collection.mutable

import ammonite.util.Classpath

/**
 * The content of a JAR that Ammonite can only reach through a URL, that the compiler class
 * path of [[PlatformMaker]] reads, through dotty `AbstractFile`s wrapping it.
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
 * This doesn't depend on dotty itself, as `AbstractFile` changed too much in 3.10.0 for a
 * single implementation of it to work with all the compilers we support - see
 * [[UrlJarFiles]] and its 3.10.0 counterpart for those.
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
  final class Dir(val name: String, val path: String, val parent: Dir | Null) {
    private val dirs0 = mutable.LinkedHashMap.empty[String, Dir]
    private val files0 = mutable.LinkedHashMap.empty[String, File]

    private[UrlJarArchive] def add(entry: ZipEntry, input: () => InputStream, size: Long): Unit = {
      val elems = entry.getName.split('/').filter(_.nonEmpty)
      if (elems.nonEmpty) {
        val dir = elems.init.foldLeft(this) { (dir, elem) =>
          dir.dirs0.getOrElseUpdate(elem, new Dir(elem, dir.path + elem + "/", dir))
        }
        val fileName = elems.last
        dir.files0(fileName) =
          new File(fileName, entry.getName, dir, input, size, entry.getTime)
      }
    }

    /** The sub-directories of this directory, by name */
    def dirs: collection.Map[String, Dir] = dirs0
    /** The files of this directory, by name */
    def files: collection.Map[String, File] = files0

    override def toString: String = s"Dir($name, $path)"
  }

  /**
   * A file of the JAR, `path` being its path in the JAR, like `scala/Predef.class`
   *
   * @param size its size, or a negative value if it is unknown
   */
  final class File(
      val name: String,
      val path: String,
      val parent: Dir,
      read: () => InputStream,
      val size: Long,
      val time: Long
  ) {
    def input(): InputStream = read()

    override def toString: String = s"File($path)"
  }
}
