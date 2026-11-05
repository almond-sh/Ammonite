package ammonite.compiler.internal

import java.nio.charset.StandardCharsets

import scala.io.Codec

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.{AbstractFile, VirtualFile}

/** The [[SourceFileMaker]] for the compilers of 3.10.0 and later. */
class SourceFileMaker3_10_0 extends SourceFileMaker {
  // what SourceFile.virtual uses as the source root of the source files it creates
  private def virtualRoot: AbstractFile =
    new VirtualFile("_root_", Array.emptyByteArray)

  def virtual(path: String, content: Array[Byte]): SourceFile =
    new SourceFile(new VirtualFile(path, content), virtualRoot, Codec.UTF8)
  def inSourceRoot(path: String)(using Context): SourceFile = {
    val root = summon[Context].settings.sourceroot.value
    val file = AbstractFile.getFile(root.jpath.nn.resolve(path))
    new SourceFile(file, root, Codec.UTF8)
  }
  def withContent(source: SourceFile, content: Array[Char]): SourceFile = {
    val bytes = new String(content).getBytes(StandardCharsets.UTF_8)
    new SourceFile(new VirtualFile(source.file.nn.path, bytes), virtualRoot, Codec.UTF8)
  }
}
