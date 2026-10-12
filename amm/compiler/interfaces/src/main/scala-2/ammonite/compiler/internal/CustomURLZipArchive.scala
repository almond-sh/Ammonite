package ammonite.compiler.internal

import java.io.{ByteArrayInputStream, InputStream}
import java.net.URL
import java.util.concurrent.ConcurrentHashMap
import java.util.zip.{ZipEntry, ZipFile, ZipInputStream}

import ammonite.util.Classpath

import scala.reflect.io.{AbstractFile, Streamable, VirtualFile}

// Adapted from scala.reflect.io.URLZipArchive.
// The only relevant change here is that it doesn't rely on
// java.util.zip.ZipEntry.getSize to get the actual size of
// a zip entry, as it is usually -1 (size unknown).
// Plus allDirsByDottedName was added for scala 2.12, and dirs is memo-ized via a lazy val.
//
// When the URL points to the root of a JAR that a JarURLConnection can open, like those of
// the JARs embedded in coursier standalone launchers, the entries are listed from that JAR
// and only read when the compiler asks for them. Otherwise, the whole JAR is read and
// decompressed upfront.
//
// Get instances via CustomURLZipArchive.apply, which shares them between the compiler
// instances Ammonite creates along a session.
final class CustomURLZipArchive(val url: java.net.URL) extends AbstractFile with Equals { self =>

  override def toURL = url

  def file: java.io.File = null

  private def dirPath(path: String) = path.split('/').toSeq.filter(_.nonEmpty)
  private def dirName(path: String) = splitPath(path, front = true)
  private def baseName(path: String) = splitPath(path, front = false)
  private def splitPath(path0: String, front: Boolean): String = {
    val isDir = path0.charAt(path0.length - 1) == '/'
    val path = if (isDir) path0.substring(0, path0.length - 1) else path0
    val idx = path.lastIndexOf('/')

    if (idx < 0)
      if (front) "/"
      else path
    else if (front) path.substring(0, idx + 1)
    else path.substring(idx + 1)
  }
  override def underlyingSource = Some(this)
  def isDirectory = true
  def lookupName(name: String, directory: Boolean) = unsupported()
  def lookupNameUnchecked(name: String, directory: Boolean) = unsupported()
  def create() = unsupported()
  def delete() = unsupported()
  def output = unsupported()
  def container = unsupported()
  def absolute = unsupported()

  abstract class Entry(path: String) extends VirtualFile(baseName(path), path) {
    // have to keep this name for compat with sbt's compiler-interface
    def getArchive: ZipFile = null
    override def underlyingSource = Some(self)
    override def toString = self.path + "(" + path + ")"
  }

  final class DirEntry(path: String) extends Entry(path) {
    val entries = collection.mutable.HashMap[String, Entry]()

    override def isDirectory = true
    override def iterator: Iterator[Entry] = entries.valuesIterator
    override def lookupName(name: String, directory: Boolean): Entry = {
      if (directory) entries(name + "/")
      else entries(name)
    }
  }

  private object Entry {

    final class EmptyFile(name: String) extends Entry(name) {
      override def toByteArray: Array[Byte] = null
      override def sizeOption = Some(0)
    }

    final class File(name: String, content: Array[Byte]) extends Entry(name) {
      override val toByteArray: Array[Byte] = content
      private def initContent(): Unit = {
        // this initializes VirtualFile.content (private field),
        // which is required in 2.12.11, for the newly added AbstractFile.unsafeToByteArray
        val os = output
        os.write(content)
        os.close()
      }
      if (CustomURLZipArchive.is2_12_11)
        initContent()
      override def sizeOption = Some(toByteArray.length)
      // Seems we have to provide that one in 2.13
      override def input: InputStream =
        new ByteArrayInputStream(content)
    }

    final class JarFileEntry(zipFile: ZipFile, zipEntry: ZipEntry)
        extends Entry(zipEntry.getName) {
      override def toByteArray: Array[Byte] = Streamable.bytes(input)
      override def sizeOption =
        if (zipEntry.getSize >= 0) Some(zipEntry.getSize.toInt) else None
      override def lastModified = zipEntry.getTime
      override def input: InputStream = zipFile.getInputStream(zipEntry)
    }
  }

  private def ensureDir(
      dirs: collection.mutable.Map[Seq[String], DirEntry],
      path: String,
      zipEntry: ZipEntry
  ): DirEntry =
    dirs.get(dirPath(path)) match {
      case Some(v) => v
      case None =>
        val parent = ensureDir(dirs, dirName(path), null)
        val dir = new DirEntry(path)
        parent.entries(baseName(path)) = dir
        dirs(dirPath(path)) = dir
        dir
    }

  private def getDir(
      dirs: collection.mutable.Map[Seq[String], DirEntry],
      entry: ZipEntry
  ): DirEntry = {
    if (entry.isDirectory) ensureDir(dirs, entry.getName, entry)
    else ensureDir(dirs, dirName(entry.getName), null)
  }

  private lazy val dirs: Map[Seq[String], DirEntry] =
    Classpath.openJar(url) match {
      // 2.12.11 needs the content of entries upfront (see Entry.File)
      case Some(zipFile) if !CustomURLZipArchive.is2_12_11 => jarFileDirs(zipFile)
      case _ => streamDirs()
    }

  private def jarFileDirs(zipFile: ZipFile): Map[Seq[String], DirEntry] = {
    val root = new DirEntry("/")
    val dirs = collection.mutable.HashMap[Seq[String], DirEntry](Nil -> root)
    val it = zipFile.entries()
    while (it.hasMoreElements) {
      val zipEntry = it.nextElement()
      val dir = getDir(dirs, zipEntry)
      if (!zipEntry.isDirectory) {
        // unlike those of a ZipInputStream, the entries of a ZipFile know their size
        val f =
          if (zipEntry.getSize == 0) new Entry.EmptyFile(zipEntry.getName)
          else new Entry.JarFileEntry(zipFile, zipEntry)
        dir.entries(f.name) = f
      }
    }
    dirs.toMap
  }

  private def streamDirs(): Map[Seq[String], DirEntry] = {
    val root = new DirEntry("/")
    val dirs = collection.mutable.HashMap[Seq[String], DirEntry](Nil -> root)
    val in = new ZipInputStream(new ByteArrayInputStream(Streamable.bytes(input)))

    @annotation.tailrec
    def loop() {
      val zipEntry = in.getNextEntry

      if (zipEntry != null) {
        val dir = getDir(dirs, zipEntry)
        if (!zipEntry.isDirectory) {
          val f =
            if (zipEntry.getSize == 0)
              new Entry.EmptyFile(zipEntry.getName)
            else {
              val content = {
                val baos = new java.io.ByteArrayOutputStream
                val b = Array.ofDim[Byte](16 * 1024)

                def loop() {
                  val read = in.read(b, 0, b.length)
                  if (read >= 0) {
                    baos.write(b, 0, read)
                    loop()
                  }
                }
                loop()

                baos.toByteArray
              }
              new Entry.File(zipEntry.getName, content)
            }
          dir.entries(f.name) = f
        }
        in.closeEntry()
        loop()
      }
    }

    loop()
    dirs.toMap
  }

  def iterator: Iterator[AbstractFile] =
    dirs(Nil).iterator

  def name = url.getFile
  def path = url.getPath
  def input = url.openStream()
  def lastModified =
    try url.openConnection().getLastModified
    catch { case _: java.io.IOException => 0 }

  override def canEqual(other: Any) = other.isInstanceOf[CustomURLZipArchive]
  override def hashCode() = url.hashCode
  override def equals(that: Any) = that match {
    case x: CustomURLZipArchive => url == x.url
    case _ => false
  }

  lazy val allDirsByDottedName: collection.Map[String, DirEntry] =
    dirs.map {
      case (k, v) =>
        k.mkString(".") -> v
    }

}

object CustomURLZipArchive {
  def closeZipFile = false

  // Keyed by the URL string, as URL#equals and URL#hashCode can resolve host names
  private val cache = new ConcurrentHashMap[String, CustomURLZipArchive]

  /**
   * The archive of `url`, shared between the compiler instances Ammonite creates, so that
   * re-creating a compiler (upon adding dependencies, for example) doesn't list or read the
   * JARs of the class path again.
   */
  def apply(url: URL): CustomURLZipArchive = {
    val key = url.toExternalForm
    val existing = cache.get(key)
    if (existing == null) {
      val archive = new CustomURLZipArchive(url)
      val previous = cache.putIfAbsent(key, archive)
      if (previous == null) archive else previous
    } else
      existing
  }

  private val is2_12_11 = scala.util.Properties.versionNumberString == "2.12.11"
}
