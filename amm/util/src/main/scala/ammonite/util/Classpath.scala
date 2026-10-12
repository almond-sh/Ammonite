package ammonite.util

import java.io.File
import java.net.{JarURLConnection, URI, URL}
import java.nio.file.{FileSystem, FileSystems}
import java.util.zip.{ZipFile, ZipInputStream}

import scala.util.control.NonFatal

/**
 * Loads the jars that make up the classpath of the scala-js-fiddle
 * compiler and re-shapes it into the correct structure to satisfy
 * scala-compile and scalajs-tools
 */
object Classpath {
  val traceClasspathIssues =
    sys.props
      .get("ammonite.trace-classpath")
      .exists(_.toLowerCase == "true")

  /**
   * The modules of the JDK we run on, which the compilers have to read the JDK classes from
   * on top of [[classpath]] - empty on Java 8, whose JDK classes [[classpath]] already lists.
   */
  lazy val jrtFileSystem: Option[FileSystem] =
    if (System.getProperty("sun.boot.class.path") == null)
      Some(FileSystems.getFileSystem(URI.create("jrt:/")))
    else
      None

  /**
   * In memory cache of all the jars used in the compiler. This takes up some
   * memory but is better than reaching all over the filesystem every time we
   * want to do something.
   */
  def classpath(classLoader: ClassLoader): Vector[URL] = {
    var current = classLoader
    val files = collection.mutable.Buffer.empty[java.net.URL]
    val seenClassLoaders = collection.mutable.Buffer.empty[ClassLoader]
    while (current != null) {
      seenClassLoaders.append(current)
      current match {
        case t: java.net.URLClassLoader =>
          files.appendAll(t.getURLs)
        case _ =>
      }
      current = current.getParent
    }

    val sunBoot = System.getProperty("sun.boot.class.path")
    if (sunBoot != null) {
      files.appendAll(
        sunBoot
          .split(java.io.File.pathSeparator)
          .map(new java.io.File(_))
          .filter(_.exists())
          .map(_.toURI.toURL)
      )
    } else if (seenClassLoaders.contains(ClassLoader.getSystemClassLoader)) {
      for (
        p <- System.getProperty("java.class.path")
          .split(File.pathSeparatorChar) if !p.endsWith("sbt-launch.jar")
      ) {
        val f = new File(p)
        if (f.exists())
          files.append(f.toURI.toURL)
      }
    }
    files.toVector
  }

  /**
   * The JAR at the root of `url`, if `url` is a `jar:` URL that points to the root of a JAR.
   *
   * Coursier standalone launchers embed the JARs of the class path, and hand out
   * `jar:file:/…/launcher.jar!/coursier/bootstrap/launcher/jars/foo.jar!/` URLs for them.
   * Opening those through their `JarURLConnection` gives a `ZipFile` that reads entries
   * on demand, from the class path JARs the launcher already opened, whereas reading
   * `url.openStream()` decompresses the whole JAR.
   *
   * The returned `ZipFile` is shared with the URL connection cache, so it mustn't be closed.
   */
  def openJar(url: URL): Option[ZipFile] =
    if (url.getProtocol == "jar")
      try {
        url.openConnection() match {
          // no entry name (null for the JDK, empty for coursier launchers): the URL points
          // to the root of the JAR
          case conn: JarURLConnection
              if conn.getEntryName == null || conn.getEntryName.isEmpty =>
            conn.setUseCaches(true)
            Option(conn.getJarFile)
          case _ => None
        }
      } catch {
        case NonFatal(e) =>
          traceClasspathProblem(s"Classpath element '$url' could not be opened as a JAR: $e")
          None
      }
    else
      None

  def canBeOpenedAsJar(url: URL): Boolean = {
    var zis: ZipInputStream = null
    try {
      if (openJar(url).nonEmpty)
        true
      else if (url.getProtocol == "file") {
        // this ignores any preamble in particular, unlike ZipInputStream
        val zf = new ZipFile(new File(url.toURI))
        zf.close()
        true
      } else {
        zis = new ZipInputStream(url.openStream())
        zis.getNextEntry != null
      }
    } catch {
      case NonFatal(e) =>
        traceClasspathProblem(
          s"Classpath element '$url' " +
            s"could not be opened as jar file because of $e"
        )
        false
    } finally {
      if (zis != null)
        zis.close()
    }
  }
  def traceClasspathProblem(msg: String): Unit =
    if (traceClasspathIssues) println(msg)
}
