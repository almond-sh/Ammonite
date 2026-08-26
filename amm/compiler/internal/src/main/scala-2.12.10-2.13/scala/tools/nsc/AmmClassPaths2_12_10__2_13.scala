package scala.tools.nsc

import java.io.File
import java.net.URL

import ammonite.compiler.internal.CustomURLZipArchive
import ammonite.util.Util

import scala.reflect.io.AbstractFile
import scala.tools.nsc.classpath.FileUtils.AbstractFileOps
import scala.tools.nsc.classpath.{ClassPathEntries, _}
import scala.tools.nsc.util.{ClassPath, ClassRepresentation}

/**
 * The class path of a JAR Ammonite can only reach through a URL.
 *
 * Originally based on
 * https://github.com/scala/scala/blob/329deac9ab4f39e5e766ec3ab3f3f4cddbc44aa1
 * /src/compiler/scala/tools/nsc/classpath/ZipAndJarFileLookupFactory.scala#L50-L166,
 * then adapted to rely on CustomURLZipArchive, which accepts URLs, rather than on
 * FileZipArchive, which only accepts files.
 */
final class UrlZipArchiveClassPath2_12_10__2_13(val zipUrl: URL)
    extends ClassPath
    with NoSourcePaths {

  def zipFile: File = null

  override def asURLs: Seq[URL] = Seq(zipUrl)

  override def asClassPathStrings: Seq[String] = Seq(zipUrl.toURI.toASCIIString) // ???

  private val archive = new CustomURLZipArchive(zipUrl)

  private def findDirEntry(pkg: String): Option[archive.DirEntry] =
    archive.allDirsByDottedName.get(pkg)

  private def isRequiredFileType(file: AbstractFile): Boolean =
    !file.isDirectory && file.hasExtension("class")

  private def createFileEntry(file: CustomURLZipArchive#Entry): ClassFileEntryImpl =
    ClassFileEntryImpl(file)

  private def filesIn(inPackage: String): Seq[ClassFileEntryImpl] =
    for {
      dirEntry <- findDirEntry(inPackage).toSeq
      entry <- dirEntry.iterator if isRequiredFileType(entry)
    } yield createFileEntry(entry)

  private def fileIn(inPackage: String, name: String): Option[ClassFileEntryImpl] =
    for {
      dirEntry <- findDirEntry(inPackage)
      entry <- Option(dirEntry.lookupName(name, directory = false))
      if isRequiredFileType(entry)
    } yield createFileEntry(entry)

  private def packagesIn(inPackage: String): Seq[PackageEntry] = {
    val prefix = PackageNameUtils.packagePrefix(inPackage)
    for {
      dirEntry <- findDirEntry(inPackage).toSeq
      entry <- dirEntry.iterator if entry.isPackage
    } yield PackageEntryImpl(prefix + entry.name)
  }

  private def listIn(inPackage: String): ClassPathEntries =
    findDirEntry(inPackage).map { dirEntry =>
      val pkgBuf = collection.mutable.ArrayBuffer.empty[PackageEntry]
      val fileBuf = collection.mutable.ArrayBuffer.empty[ClassFileEntryImpl]
      val prefix = PackageNameUtils.packagePrefix(inPackage)

      for (entry <- dirEntry.iterator) {
        if (entry.isPackage)
          pkgBuf += PackageEntryImpl(prefix + entry.name)
        else if (isRequiredFileType(entry))
          fileBuf += createFileEntry(entry)
      }
      ClassPathEntries(pkgBuf, fileBuf)
    }.getOrElse(ClassPathEntries(Nil, Nil))

  override def findClassFile(className: String): Option[AbstractFile] = {
    val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
    fileIn(pkg, simpleClassName + ".class").map(_.file)
  }

  // This method is performance sensitive as it is used by SBT's ExtractDependencies phase.
  override def findClass(className: String): Option[ClassRepresentation] = {
    val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
    fileIn(pkg, simpleClassName + ".class")
  }

  def packages(inPackage: PackageName): Seq[PackageEntry] = packagesIn(inPackage.dottedString)
  def list(inPackage: PackageName): ClassPathEntries = listIn(inPackage.dottedString)
  def classes(inPackage: PackageName): Seq[ClassFileEntry] = filesIn(inPackage.dottedString)
  def hasPackage(pkg: PackageName): Boolean = findDirEntry(pkg.dottedString).isDefined
}

/** An aggregate class path cut down to the classes a white list lets through. */
final class WhiteListClassPath2_12_10__2_13(
    aggregates: Seq[ClassPath],
    whitelist: Set[Seq[String]]
) extends scala.tools.nsc.classpath.AggregateClassPath(aggregates) {
  override def findClassFile(name: String): Option[AbstractFile] = {
    val tokens = name.split('.')
    if (Util.lookupWhiteList(whitelist, tokens.init ++ Seq(tokens.last + ".class")))
      super.findClassFile(name)
    else None
  }
  override def list(inPackage: PackageName): ClassPathEntries = {
    val superList = super.list(inPackage)
    ClassPathEntries(
      superList.packages.filter(p => Util.lookupWhiteList(whitelist, p.name.split('.'))),
      superList.classesAndSources.filter { t =>
        val pkg = inPackage.dottedString
        Util.lookupWhiteList(whitelist, pkg.split('.') ++ Seq(t.name + ".class"))
      }
    )
  }

  override def toString: String =
    s"WhiteListClassPath($aggregates, ${whitelist.size} white-listed elements)"
}
