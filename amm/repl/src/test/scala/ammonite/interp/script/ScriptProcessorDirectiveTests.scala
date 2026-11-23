package ammonite.interp.script

import ammonite.compiler.{CompilerBuilder, DefaultCodeWrapper, Parsers}
import ammonite.interp.DependencyLoader
import ammonite.runtime.Storage
import ammonite.util.{Name, Printer}
import ammonite.util.Util.CodeSource
import utest._

object ScriptProcessorDirectiveTests extends TestSuite {

  private val wd = os.pwd

  private def processor = {
    val printer = Printer(
      System.out,
      System.err,
      System.out,
      _ => (),
      _ => (),
      _ => ()
    )
    ScriptProcessor(
      CompilerBuilder().scalaVersion,
      Parsers,
      DefaultCodeWrapper,
      new DependencyLoader(printer, Storage.InMemory(), Nil, verboseOutput = false),
      Nil,
      wd = wd
    )
  }

  private def load(code: String): Script = {
    val path = wd / "amm" / "src" / "test" / "resources" / "importHooks" / "Script.sc"
    processor.load(code, CodeSource(Name("Script"), Nil, Seq(Name("$file")), Some(path.toNIO)))
  }

  val tests = Tests {
    test("dependencies") {
      val script = load(
        """//> using dep com.lihaoyi::scalatags:0.12.0
          |//> using repository jitpack
          |//> using options -deprecation -feature
          |
          |val n = 2
          |""".stripMargin
      )
      assert(script.processorDiagnostics.isEmpty)
      val deps = script.dependencies.dependencies.map(_.getModule.getName)
      assert(deps.exists(_.startsWith("scalatags_")))
      assert(script.dependencies.extraRepositories.nonEmpty)
      assert(script.options.extraScalacOptions == Seq("-deprecation", "-feature"))
    }

    test("scripts") {
      val script = load("//> using script Basic.sc\n\nval n = Basic.basicValue\n")
      assert(script.processorDiagnostics.isEmpty)
      val scripts = script.dependencies.scriptDependencies.flatMap(_.code.toOption)
      assert(scripts.map(_.last) == Seq("Basic.sc"))
    }

    test("diagnostics") {
      val script = load(
        """//> using foo bar
          |//> using script DoesNotExist.sc
          |""".stripMargin
      )
      val diagnostics = script.processorDiagnostics.map(d => (d.severity, d.start.line))
      assert(diagnostics == Seq(("WARNING", 0), ("ERROR", 1)))
    }
  }
}
