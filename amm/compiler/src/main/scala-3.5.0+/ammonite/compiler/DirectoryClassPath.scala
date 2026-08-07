package dotty.ammonite.compiler

import java.io.{File => JFile}
import dotty.tools.dotc.classpath
import dotty.tools.io.{AbstractFile, PlainFile, ClassPath, ClassRepresentation, EfficientClassPath}
import classpath.FileUtils._

case class DirectoryClassPath(dir: JFile)
    extends classpath.JFileDirectoryLookup[classpath.BinaryFileEntry]
    with classpath.NoSourcePaths {

  override def findClassFile(className: String): Option[AbstractFile] = {
    val relativePath = classpath.FileUtils.dirPath(className)
    val classFile = new JFile(dir, relativePath + ".class")
    if (classFile.exists) {
      val wrappedClassFile = new dotty.tools.io.File(classFile.toPath)
      val abstractClassFile = new PlainFile(wrappedClassFile)
      Some(abstractClassFile)
    } else None
  }

  override def findClassFileAndModuleFile(
      className: String,
      findModule: Boolean
  ): Option[(AbstractFile, Option[AbstractFile])] =
    findClassFile(className).map { classFile =>
      val moduleFile = Option
        .when(findModule)(new JFile(dir, "module-info.class"))
        .filter(_.exists)
        .map(file => new PlainFile(new dotty.tools.io.File(file.toPath)))
      (classFile, moduleFile)
    }

  protected def createFileEntry(file: AbstractFile): classpath.BinaryFileEntry =
    classpath.BinaryFileEntry(file)
  protected def isMatchingFile(f: JFile): Boolean =
    f.isClass

  private[dotty] def classes(inPackage: classpath.PackageName): Seq[classpath.BinaryFileEntry] =
    files(inPackage)
}
