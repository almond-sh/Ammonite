package ammonite.compiler.internal

import java.net.URL

import scala.reflect.io.FileZipArchive
import scala.tools.nsc.Settings
import scala.tools.nsc.classpath.ZipAndJarClassPathFactory
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.{UrlZipArchiveClassPath2_12_10__2_13, WhiteListClassPath2_12_10__2_13}

/** The [[ClassPathMaker]] for the scalac class paths of 2.12.10 to 2.13.x. */
class ClassPathMaker2_12_10__2_13 extends ClassPathMaker {

  def zipClassPath(archive: FileZipArchive, settings: Settings): ClassPath =
    ZipAndJarClassPathFactory.create(archive, settings, new scala.tools.nsc.CloseableRegistry())

  def urlZipClassPath(url: URL, settings: Settings): ClassPath =
    new UrlZipArchiveClassPath2_12_10__2_13(url)

  def whiteListClassPath(classPaths: Seq[ClassPath], whiteList: Set[Seq[String]]): ClassPath =
    new WhiteListClassPath2_12_10__2_13(classPaths, whiteList)
}
