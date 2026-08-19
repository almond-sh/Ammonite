package ammonite.compiler

import java.nio.file.Path

import ammonite.compiler.iface.{
  CompilerBuilder => ICompilerBuilder,
  CompilerBuilderFactory,
  Parser
}

/**
 * Exposes this module's compiler to the modules that can't depend on it directly,
 * see [[ammonite.compiler.iface.CompilerBuilderFactory]].
 *
 * Registered in `META-INF/services/ammonite.compiler.iface.CompilerBuilderFactory`.
 */
class AmmoniteCompilerBuilderFactory extends CompilerBuilderFactory {
  def scalaVersion: String = CompilerBuilder.scalaVersion
  def compilerBuilder(outputDir: Option[Path]): ICompilerBuilder = CompilerBuilder(outputDir)
  def parser: Parser = Parsers
}
