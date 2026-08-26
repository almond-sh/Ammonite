package ammonite.compiler.internal

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.util.SourceFile

/**
 * Creates the compilation units Ammonite feeds the compiler.
 *
 * `CompilationUnit` took a second constructor parameter in 3.4.2.
 */
trait CompilationUnitMaker {

  /** A compilation unit for `source` that the compiler will not try to suspend. */
  def notSuspendable(source: SourceFile): CompilationUnit
}
