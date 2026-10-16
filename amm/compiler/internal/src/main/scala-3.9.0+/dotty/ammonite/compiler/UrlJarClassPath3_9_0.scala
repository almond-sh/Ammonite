package dotty.ammonite.compiler

import java.net.URL

import ammonite.compiler.internal.UrlJarArchive
import dotty.tools.dotc.classpath.FileUtils.*
import dotty.tools.dotc.classpath.*
import dotty.tools.io.{AbstractFile, ClassPath}

/**
 * The class path of a JAR that Ammonite can only reach through a URL, for the compilers of
 * 3.9.0 and later.
 *
 * Modelled after the `ZipArchiveClassPath` of dotty's `ZipAndJarClassPathFactory`, that only
 * accepts JARs from the file system.
 */
final class UrlJarClassPath3_9_0(url: URL, root: UrlJarArchive.Dir)
    extends ClassPath
    with NoSourcePaths {

  // the directories of the packages of the JAR, by package name
  private lazy val packageDirs: Map[String, AbstractFile] = {
    val b = Map.newBuilder[String, AbstractFile]
    def add(prefix: String, dir: AbstractFile): Unit =
      for (sub <- dir.iterator if sub.isPackage) {
        val name = prefix + sub.name
        b += name -> sub
        add(name + ".", sub)
      }
    b += ClassPath.RootPackage -> root
    add("", root)
    b.result()
  }

  private def file(pkg: String, name: String): Option[AbstractFile] =
    packageDirs.get(pkg).flatMap(dir => Option(dir.lookupName(name, directory = false)))

  def asURLs: Seq[URL] = Seq(url)
  def asClassPathStrings: Seq[String] = Seq(url.toExternalForm)

  private[dotty] def hasPackage(pkg: PackageName): Boolean =
    packageDirs.contains(pkg.dottedString)
  private[dotty] def packages(inPackage: PackageName): Seq[PackageEntry] =
    packageDirs.get(inPackage.dottedString).toSeq.flatMap { dir =>
      dir.iterator.filter(_.isPackage).map(sub => PackageEntryImpl(inPackage.entryName(sub.name)))
    }
  private[dotty] def classes(inPackage: PackageName): Seq[BinaryFileEntry] =
    packageDirs.get(inPackage.dottedString).toSeq.flatMap { dir =>
      dir.iterator
        .filter(f => f.isTasty || (f.isClass && !f.hasSiblingTasty))
        .map(BinaryFileEntry(_))
    }
  private[dotty] def list(inPackage: PackageName): ClassPathEntries =
    ClassPathEntries(packages(inPackage), classes(inPackage))

  def findClassFileAndModuleFile(
      className: String,
      findModule: Boolean
  ): Option[(AbstractFile, Option[AbstractFile])] = {
    val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
    file(pkg, simpleClassName + ".class").map { classFile =>
      val moduleFile = if (findModule) file(pkg, "module-info.class") else None
      (classFile, moduleFile)
    }
  }

  override def toString: String = s"UrlJarClassPath($url)"
}
