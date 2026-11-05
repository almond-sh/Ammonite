package dotty.ammonite.compiler

import ammonite.util.Util

import dotty.tools.dotc.classpath.{ClassPathEntries, PackageName}
import dotty.tools.io.ClassPath

/**
 * The class path of the parts of `aggregates` that `whitelist` lists, for the compilers of 3.9.x.
 */
class WhiteListClassPath3_9(aggregates: Seq[ClassPath], whitelist: Set[Seq[String]])
    extends dotty.tools.dotc.classpath.AggregateClassPath(aggregates) {
  // findClassFile goes through findClassFileAndModuleFile since 3.9.0
  override def findClassFileAndModuleFile(name: String, findModule: Boolean) = {
    val tokens = name.split('.')
    if (Util.lookupWhiteList(whitelist, tokens.init ++ Seq(tokens.last + ".class"))) {
      super.findClassFileAndModuleFile(name, findModule)
    } else None
  }

  override def list(inPackage: PackageName) = {
    val superList = super.list(inPackage)
    ClassPathEntries(
      superList.packages.filter { p => Util.lookupWhiteList(whitelist, p.name.split('.')) },
      superList.classesAndSources.filter { t =>
        Util.lookupWhiteList(whitelist, inPackage.dottedString.split('.') ++ Seq(t.name + ".class"))
      }
    )
  }

  override def toString: String =
    s"WhiteListClassPath($aggregates, ${whitelist.size} white-listed elements)"
}
