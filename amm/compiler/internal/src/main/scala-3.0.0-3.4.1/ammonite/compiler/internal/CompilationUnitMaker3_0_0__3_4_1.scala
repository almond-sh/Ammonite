package ammonite.compiler.internal

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.util.SourceFile

/** The [[CompilationUnitMaker]] for the compilers of 3.0.0 to 3.4.1. */
class CompilationUnitMaker3_0_0__3_4_1 extends CompilationUnitMaker {
  def notSuspendable(source: SourceFile): CompilationUnit =
    // as done in
    // https://github.com/lampepfl/dotty/blob/3.0.0-M3/
    //   compiler/src/dotty/tools/repl/ReplCompillationUnit.scala/#L8
    new CompilationUnit(source):
      override def isSuspendable: Boolean = false
}
