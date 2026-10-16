package dotty.ammonite.compiler

import java.net.URL

import ammonite.compiler.internal.UrlJarArchive
import dotty.tools.dotc.classpath.FileUtils._
import dotty.tools.dotc.classpath._
import dotty.tools.io.{AbstractFile, ClassPath, ClassRepresentation}

/**
 * The class path of a JAR that Ammonite can only reach through a URL, for the compilers up
 * to 3.3.x.
 *
 * Modelled after the `ZipArchiveClassPath` of dotty's `ZipAndJarClassPathFactory`, that only
 * accepts JARs from the file system.
 */
final class UrlJarClassPath3_0_0__3_3(url: URL, root: UrlJarArchive.Dir)
    extends ClassPath
    with NoSourcePaths {

  // the directories of the packages of the JAR, by package name
  private lazy val packageDirs: Map[String, AbstractFile] = {
    val b = Map.newBuilder[String, AbstractFile]
    def add(prefix: String, dir: AbstractFile): Unit =
      for (sub <- dir.iterator() if sub.isPackage) {
        val name = prefix + sub.name
        b += name -> sub
        add(name + ".", sub)
      }
    b += ClassPath.RootPackage -> root
    add("", root)
    b.result()
  }

  private def classFile(className: String): Option[AbstractFile] = {
    val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
    packageDirs.get(pkg).flatMap { dir =>
      Option(dir.lookupName(simpleClassName + ".class", directory = false))
    }
  }

  def asURLs: Seq[URL] = Seq(url)
  def asClassPathStrings: Seq[String] = Seq(url.toExternalForm)

  private[dotty] def hasPackage(pkg: PackageName): Boolean =
    packageDirs.contains(pkg.dottedString)
  private[dotty] def packages(inPackage: PackageName): Seq[PackageEntry] =
    packageDirs.get(inPackage.dottedString).toSeq.flatMap { dir =>
      dir.iterator().filter(_.isPackage).map(sub => PackageEntryImpl(inPackage.entryName(sub.name)))
    }
  private[dotty] def classes(inPackage: PackageName): Seq[ClassFileEntry] =
    packageDirs.get(inPackage.dottedString).toSeq.flatMap { dir =>
      dir.iterator().filter(_.isClass).map(ClassFileEntryImpl(_))
    }
  private[dotty] def list(inPackage: PackageName): ClassPathEntries =
    ClassPathEntries(packages(inPackage), classes(inPackage))

  def findClassFile(className: String): Option[AbstractFile] =
    classFile(className)
  override def findClass(className: String): Option[ClassRepresentation] =
    classFile(className).map(ClassFileEntryImpl(_))

  override def toString: String = s"UrlJarClassPath($url)"
}
