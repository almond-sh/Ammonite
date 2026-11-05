package ammonite.compiler.internal

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.util.SourceFile

/** The [[CompilationUnitMaker]] for the compilers of 3.4.2 to 3.9.x. */
class CompilationUnitMaker3_4_2__3_9 extends CompilationUnitMaker {
  def notSuspendable(source: SourceFile): CompilationUnit =
    // as done in
    // https://github.com/lampepfl/dotty/blob/3.0.0-M3/
    //   compiler/src/dotty/tools/repl/ReplCompillationUnit.scala/#L8
    new CompilationUnit(source, null):
      override def isSuspendable: Boolean = false
}
