package dotty.ammonite.compiler

import ammonite.util.Util

import dotty.tools.dotc.classpath.{AggregateClassPath, BinaryFileEntry, ClassPath, SourceFileEntry}
import dotty.tools.io.AbstractFile

/**
 * The class path of the parts of `aggregates` that `whitelist` lists, for the compilers of
 * 3.10.0 and later.
 */
class WhiteListClassPath3_10_0(aggregates: Seq[ClassPath], whitelist: Set[Seq[String]])
    extends AggregateClassPath(aggregates) {

  private def isWhiteListed(inPackage: String, name: String): Boolean =
    Util.lookupWhiteList(whitelist, inPackage.split('.').toSeq ++ Seq(name + ".class"))

  override def findClassFile(name: String): Option[AbstractFile] = {
    val tokens = name.split('.')
    if (Util.lookupWhiteList(whitelist, tokens.init.toSeq ++ Seq(tokens.last + ".class")))
      super.findClassFile(name)
    else None
  }

  override def packages(inPackage: String): Iterable[String] =
    super.packages(inPackage).filter(p => Util.lookupWhiteList(whitelist, p.split('.').toSeq))

  override def classes(inPackage: String): Iterable[BinaryFileEntry] =
    super.classes(inPackage).filter(e => isWhiteListed(inPackage, e.name))

  override def sources(inPackage: String): Iterable[SourceFileEntry] =
    super.sources(inPackage).filter(e => isWhiteListed(inPackage, e.name))

  override def toString: String =
    s"WhiteListClassPath($aggregates, ${whitelist.size} white-listed elements)"
}
