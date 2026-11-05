package ammonite.compiler.iface

import ammonite.util.{Imports, Printer}

abstract class Compiler {

  /**
   * @param importsLen length of the code Ammonite generated before the user code in `src`
   * @param suffixLen length of the code Ammonite generated after the user code in `src`
   */
  def compile(
      src: Array[Byte],
      printer: Printer,
      importsLen: Int,
      userCodeNestingLevel: Int,
      fileName: String,
      suffixLen: Int
  ): Option[Compiler.Output]

  def preprocessor(fileName: String, markGeneratedSections: Boolean = false): Preprocessor

}

object Compiler {

  case class Output(
      classFiles: Vector[(String, Array[Byte])],
      imports: Imports,
      usedEarlierDefinitions: Option[Seq[String]]
  )

}
