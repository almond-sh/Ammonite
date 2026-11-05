package ammonite.compiler.internal

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.CompilationUnitInfo
import dotty.tools.dotc.util.SourceFile

/** The [[CompilationUnitMaker]] for the compilers of 3.10.0 and later. */
class CompilationUnitMaker3_10_0 extends CompilationUnitMaker {
  def notSuspendable(source: SourceFile): CompilationUnit =
    // Unlike with the earlier compilers, the classes of the unit need to know their source
    // file, that is, the unit needs an info: since 3.10.0, ExpandPrivate checks that the
    // private methods it makes public are accessed from their own source file, and the
    // lambdas that mainargs' macros expand to are such methods.
    new CompilationUnit(source, new CompilationUnitInfo(source.file)):
      override def isSuspendable: Boolean = false
}
