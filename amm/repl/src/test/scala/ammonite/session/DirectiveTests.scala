package ammonite.session

import ammonite.{DualTestRepl, TestRepl}
import ammonite.runtime.DirectiveHook
import ammonite.util.Res
import utest._

object DirectiveTests extends TestSuite {

  private val hooksDir = "amm/src/test/resources/importHooks"

  val tests = Tests {
    println("DirectiveTests")
    val check = new DualTestRepl()

    /**
     * Runs a command spanning several lines (that sessions can't hold, as they expect each
     * line but the last to be incomplete code)
     */
    def run(repl: TestRepl, code: String) = {
      val res = repl.run(code, repl.currentLine)
      res._1 match {
        case _: Res.Failing => assert({ identity(res); false })
        case _ =>
      }
      res
    }

    // commands holding only directives compile nothing, so they don't get a result number
    test("script") {
      test("basic") - check.session(s"""
        @ //> using script $hooksDir/Basic.sc

        @ Basic.basicValue
        res0: Int = 31337
      """)

      test("noExtension") - check.session(s"""
        @ //> using script $hooksDir/Basic

        @ Basic.basicValue
        res0: Int = 31337
      """)

      test("several") - check.session(s"""
        @ //> using scripts $hooksDir/Basic.sc $hooksDir/BasicTwo.sc

        @ Basic.basicValue + BasicTwo.basicValueTwo
        res0: Int = 32674
      """)

      test("sameCommand") {
        for (repl <- check.repls) {
          val (_, out, res, _, _, _) = run(
            repl,
            s"""// some comment
               |//> using script $hooksDir/Basic.sc
               |
               |Basic.basicValue + 1""".stripMargin
          )
          assert((out + res).contains("31338"))
        }
      }

      test("notFound") - check.session(s"""
        @ //> using script $hooksDir/DoesNotExist.sc
        error: Script not found
      """)

      test("fromScript") - check.session(s"""
        @ import $$file.amm.src.test.resources.importHooks.DirectiveScript

        @ DirectiveScript.directiveScriptVal
        res1: Int = 32674
      """)

      test("indirect") - check.session(s"""
        @ //> using script $hooksDir/DirectiveIndirect.sc

        @ DirectiveIndirect.directiveIndirectVal
        res0: Int = 32675
      """)
    }

    test("dep") {
      test("basic") - check.session(s"""
        @ import scalatags.Text.all._
        error: ${check.notFound("scalatags")}

        @ //> using dep com.lihaoyi::scalatags:0.12.0

        @ import scalatags.Text.all._

        @ div("Hello").render
        res1: String = "<div>Hello</div>"
      """)

      test("fromScript") - check.session(s"""
        @ //> using script $hooksDir/DirectiveDep.sc

        @ DirectiveDep.rendered
        res0: String = "<div>Moo</div>"
      """)

      test("malformed") - check.session("""
        @ //> using dep com.lihaoyi
        error: (console):1:11: Invalid dependencies
      """)
    }

    test("repository") - check.session("""
      @ //> using repository jitpack

      @ interp.repositories().exists(_.toString.contains("jitpack"))
      res0: Boolean = true
    """)

    test("option") - check.session("""
      @ //> using options -language:dynamics -language:reflectiveCalls

      @ interp.scalacOptions.contains("-language:reflectiveCalls")
      res0: Boolean = true
    """)

    test("unrecognized") {
      check.session("""
        @ //> using foo bar
        error: (console):1:11: unrecognized directive '//> using foo'
      """)
      // none of the directives of a command are run if some are unrecognized
      val repl = new TestRepl
      val res = repl.run("//> using options -language:dynamics\n//> using foo bar\n", 0)
      res._1 match {
        case Res.Failure(msg) => assert(msg.contains("(console):2:11: unrecognized directive"))
        case _ => assert({ identity(res); false })
      }
      assert(!repl.interp.scalacOptions.contains("-language:dynamics"))
    }

    test("parseError") - check.session("""
      @ //> using dep "com.lihaoyi::scalatags:0.12.0
      error: (console):1:
    """)

    test("prefix") {
      // reports the keys it handles, as warnings
      def hook(name: String): DirectiveHook = new DirectiveHook {
        def handle(
            source: ammonite.util.Util.CodeSource,
            directive: ammonite.directives.UsingDirective,
            interp: ammonite.runtime.ImportHook.InterpreterInterface,
            wrapperPath: Seq[ammonite.util.Name]
        ) = Right(Seq(ammonite.runtime.ImportHook.Result.Warning(s"$name: ${directive.key}")))
      }
      val repl = new TestRepl {
        override def directiveHooks = DirectiveHook.defaults ++ Seq(
          "foo.*" -> hook("foo-prefix"),
          "foo.bar.*" -> hook("foo-bar-prefix"),
          "foo.exact" -> hook("foo-exact")
        )
      }
      repl.session("""
        @ //> using foo.thing a
        warning: (console):1:11: foo-prefix: foo.thing

        @ //> using foo.bar.thing a
        warning: (console):1:11: foo-bar-prefix: foo.bar.thing

        @ //> using foo.exact a
        warning: (console):1:11: foo-exact: foo.exact

        @ //> using foo a
        error: (console):1:11: unrecognized directive '//> using foo'
      """)
      // prefixes aren't offered as keys
      val keys = repl.interp.completeUsingDirective("//> using fo", 12).map(_._2)
      assert(keys == Some(Seq("foo.exact")))
    }

    test("disabled") {
      val repl = new TestRepl {
        override def directiveHooks = Map.empty
      }
      repl.session(s"""
        @ //> using script $hooksDir/Basic.sc

        @ Basic.basicValue
        error: ${repl.notFound("Basic")}
      """)
    }

    test("complete") {
      def complete(code: String) = {
        val pos = code.indexOf('|')
        assert(pos >= 0)
        val code0 = code.take(pos) + code.drop(pos + 1)
        check.interps.map(_.completeUsingDirective(code0, pos)).distinct match {
          case Seq(res) => res
          case other => throw new Exception(s"Different completions: $other")
        }
      }

      test("key") {
        val res = complete("//> using scr|")
        assert(res == Some((10, Seq("script", "scripts"))))
      }

      test("script") {
        val res = complete(s"//> using script $hooksDir/Dir|")
        val start = s"//> using script $hooksDir/".length
        val expected = Seq("DirectiveDep.sc", "DirectiveIndirect.sc", "DirectiveScript.sc")
        assert(res == Some((start, expected)))
      }

      test("scriptDirectory") {
        val res = complete("//> using script amm/src/test/res|")
        assert(res == Some(("//> using script amm/src/test/".length, Seq("resources/"))))
      }

      test("dependencyVersions") {
        val res = complete("//> using dep com.lihaoyi::scalatags:0.12|")
        val (start, completions) = res.getOrElse(sys.error("No completions"))
        assert(start == "//> using dep com.lihaoyi::scalatags:".length)
        assert(completions.contains("0.12.0"))
      }

      test("noHook") {
        assert(complete("//> using foo ba|") == Some((16, Nil)))
      }

      test("code") {
        assert(complete("val n = Lis|").isEmpty)
        assert(complete("//> using dep foo\nval n = Lis|").isEmpty)
      }
    }

    test("sortVersions") {
      val versions =
        Seq("2.0.0-M4", "2.0.0-RC1", "2.0.0", "2.10.0", "2.9.0", "2.13.0", "2.13.0-RC1", "2.9.0")
      val res = DirectiveHook.Dependencies.sortCompletions("org.typelevel::cats-core:", versions)
      val expected =
        Seq("2.13.0", "2.13.0-RC1", "2.10.0", "2.9.0", "2.0.0", "2.0.0-RC1", "2.0.0-M4")
      assert(res == expected)

      val names = Seq("cats-free", "cats-core", "cats-effect")
      assert(DirectiveHook.Dependencies.sortCompletions("org.typelevel::cats-", names) == names)
    }

    test("isVersion") {
      val versions = Seq(
        "org.typelevel:cats-core_2.13:",
        "org.typelevel:cats-core_2.13:2.1",
        "org.typelevel::cats-core:",
        "org.scala-lang:::scala-compiler:"
      )
      val notVersions =
        Seq("", "org.type", "org.typelevel:", "org.typelevel::cats-co", "org.typelevel:::")
      for (input <- versions)
        assert(DirectiveHook.Dependencies.isVersion(input))
      for (input <- notVersions)
        assert(!DirectiveHook.Dependencies.isVersion(input))
    }

    test("jlineCandidate") {
      val code = "//> using dep com.lihaoyi::scalatags:0.12"
      assert(ammonite.repl.FrontEnds.inDirective(code, code.length))
      assert(!ammonite.repl.FrontEnds.inDirective("val n = 2", 9))
      // JLine words are delimited by spaces, and completions start after the last ':'
      val candidate = ammonite.repl.FrontEnds.directiveCandidate(
        code,
        "//> using dep ".length,
        "//> using dep com.lihaoyi::scalatags:".length,
        "0.12.0"
      )
      assert(candidate == "com.lihaoyi::scalatags:0.12.0")
    }
  }
}
