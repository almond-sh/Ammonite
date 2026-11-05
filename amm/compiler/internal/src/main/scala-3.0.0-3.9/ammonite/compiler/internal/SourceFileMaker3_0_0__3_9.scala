package ammonite.compiler.internal

import java.nio.charset.StandardCharsets
import java.nio.file.Paths

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.{AbstractFile, VirtualFile}

/** The [[SourceFileMaker]] for the compilers up to 3.9.x. */
class SourceFileMaker3_0_0__3_9 extends SourceFileMaker {
  def virtual(path: String, content: Array[Byte]): SourceFile = {
    val vf = new VirtualFile(path.split("/", -1).last, path)
    val out = vf.output
    out.write(content)
    out.close()
    new SourceFile(vf, new String(content, StandardCharsets.UTF_8).toCharArray)
  }
  def inSourceRoot(path: String)(using Context): SourceFile = {
    val root = summon[Context].settings.sourceroot.value
    SourceFile(AbstractFile.getFile(Paths.get(root).resolve(path)), "UTF-8")
  }
  def withContent(source: SourceFile, content: Array[Char]): SourceFile =
    new SourceFile(source.file, content)
}
