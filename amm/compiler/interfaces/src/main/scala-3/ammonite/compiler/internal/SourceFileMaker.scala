package ammonite.compiler.internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.util.SourceFile

/**
 * Creates the source files Ammonite feeds the compiler.
 *
 * 3.10.0 had `SourceFile` read its content from its file only, made `-sourceroot` a file
 * rather than a path, and changed the constructor of `VirtualFile`.
 */
trait SourceFileMaker {

  /** A source file that only lives in memory, at `path`, like `foo/cmd2.sc`. */
  def virtual(path: String, content: Array[Byte]): SourceFile

  /** The source file at `path` on disk, relative to the `-sourceroot` of the compiler. */
  def inSourceRoot(path: String)(using Context): SourceFile

  /** A source file with the same file as `source`, but whose content is `content`. */
  def withContent(source: SourceFile, content: Array[Char]): SourceFile
}
