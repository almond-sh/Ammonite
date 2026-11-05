package dotty.ammonite.compiler

import java.net.URL

import dotty.tools.dotc.classpath.FileUtils.*
import dotty.tools.dotc.classpath.*
import dotty.tools.io.AbstractFile

/**
 * The class path of a JAR that Ammonite can only reach through a URL, whose content is `root`,
 * for the compilers of 3.10.0 and later.
 *
 * Modelled after the `ZipArchiveClassPath` of dotty's `ZipAndJarClassPathFactory`, that only
 * accepts JARs from the file system.
 */
final class UrlJarClassPath3_10_0(url: URL, root: AbstractFile) extends ClassPath {

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

  override def asURLs: Seq[URL] = Seq(url)

  override def hasPackage(pkg: String): Boolean =
    packageDirs.contains(pkg)
  override def packages(inPackage: String): Iterable[String] =
    packageDirs.get(inPackage).toSeq.flatMap { dir =>
      dir.iterator.filter(_.isPackage).map(sub => PackageNameUtils.entryName(inPackage, sub.name))
    }
  override def classes(inPackage: String): Iterable[BinaryFileEntry] =
    packageDirs.get(inPackage).toSeq.flatMap { dir =>
      dir.iterator
        .filter(f => f.ext.isTasty || (f.ext.isClass && !f.hasSiblingTasty))
        .map(BinaryFileEntry(_))
    }

  override def findClassFile(className: String): Option[AbstractFile] = {
    val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
    file(pkg, simpleClassName + ".class")
  }

  override def toString: String = s"UrlJarClassPath($url)"
}
