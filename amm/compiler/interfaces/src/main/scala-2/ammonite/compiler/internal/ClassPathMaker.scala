package ammonite.compiler.internal

import java.net.URL

import scala.reflect.io.FileZipArchive
import scala.tools.nsc.Settings
import scala.tools.nsc.util.ClassPath

/**
 * Creates the class path entries Ammonite hands the Scala compiler on top of its own.
 *
 * `ZipAndJarClassPathFactory.create` takes a `CloseableRegistry` since 2.12.9, and scalac's
 * `ClassPath` methods take a `PackageName` rather than a `String` since 2.12.10.
 */
trait ClassPathMaker {

  /** The class path of a JAR that is on the file system. */
  def zipClassPath(archive: FileZipArchive, settings: Settings): ClassPath

  /** The class path of a JAR we can only reach through a URL. */
  def urlZipClassPath(url: URL, settings: Settings): ClassPath

  /** `classPaths`, cut down to the classes `whiteList` lets through. */
  def whiteListClassPath(classPaths: Seq[ClassPath], whiteList: Set[Seq[String]]): ClassPath
}
