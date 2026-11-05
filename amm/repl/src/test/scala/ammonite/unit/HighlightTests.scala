package ammonite.unit

import ammonite.compiler.CompilerBuilder.scalaVersion
import ammonite.compiler.Parsers
import utest._

object HighlightTests extends TestSuite {

  def testHighlight(buffer: Vector[Char]) = Parsers.defaultHighlight(
    buffer,
    fansi.Color.Blue,
    fansi.Color.Green,
    fansi.Color.Green,
    fansi.Color.Yellow,
    fansi.Color.Red,
    fansi.Attr.Reset
  )

  def check(source: String, expected: String) = {
    val highlighted =
      testHighlight(source.toVector)
        .mkString
        .replace(fansi.Color.Blue.escape, "<B|")
        .replace(fansi.Color.Green.escape, "<G|")
        .replace(fansi.Color.Yellow.escape, "<Y|")
        .replace(fansi.Color.Reset.escape, ">")
    assert(highlighted == expected)
  }
  val isScala2 = scalaVersion.startsWith("2.")
  val isScala3_01 = scalaVersion.startsWith("3.0.") || scalaVersion.startsWith("3.1.")
  val isScala3_10Plus = !isScala2 && {
    val Array(_, minor) = scalaVersion.split('.').take(2)
    minor.toInt >= 10
  }
  val tests = Tests {
    println("HighlightTests")
    test("highlighting") {
      test("fuzz") {

        val paths = os.walk(os.pwd).filter(_.ext == "scala")
        for (path <- paths) {
          val code = os.read(path)
          val out = Parsers.defaultHighlight(
            code.toVector,
            fansi.Underlined.On,
            fansi.Underlined.On,
            fansi.Underlined.On,
            fansi.Underlined.On,
            fansi.Underlined.On,
            fansi.Attr.Reset
          ).mkString
            .replace(fansi.Underlined.On.escape, "")
            .replace(fansi.Underlined.Off.escape, "")
          val outLength = out.length
          val codeLength = code.length

          assert { identity(path); outLength == codeLength }
          assert { identity(path); out == code }
        }
        paths.length
      }
      test("comment") - check("//a", "<B|//a>")
      test("type") - {
        val expected =
          if (isScala2) "x: <G|y>.<Y|type>"
          else "x: <G|y.type>"
        check("x: y.type", expected)
      }
      test("qualifiedType") - {
        // Scala 2 colors the whole path, Scala 3 its last segment, like dotty
        val expected =
          if (isScala2) "<Y|def> f(x: <G|java.io.File>) = x"
          else "<Y|def> f(x: java.io.<G|File>) = x"
        check("def f(x: java.io.File) = x", expected)
      }
      test("literal") - check("1", "<G|1>")
      test("expressions") - check(
        "val (x, y) = 1 + 2 + 3",
        "<Y|val> (x, y) = <G|1> + <G|2> + <G|3>"
      )
      test("interpolation") - check(
        """(s"hello ${world + 1}")""",
        """(<G|s"hello >${world + <G|1>}<G|">)"""
      )
      test("runOff") - check("""(1 + "Hello...""", """(<G|1> + <G|"Hello...>""")
      test("openStrings") {
        test("multiLine") - check("x + \"\"\"a\nb", "x + <G|\"\"\"a\nb>")
        test("escapedQuote") - check("\"a\\\"b", "<G|\"a\\\"b>")
        test("interpolated") - {
          val expected =
            if (isScala2) "<G|s\"a\\\">b"
            else "<G|s\"a\\\"b>"
          check("s\"a\\\"b", expected)
        }
        test("afterIdent") - check("s\"a $x b", "<G|s\"a >$x<G| b>")
        test("afterBlock") - check("s\"a ${x} b", "<G|s\"a >${x}<G| b>")
        test("interpolatedMultiLine") - check(
          "s\"\"\"a ${x} b\nc",
          "<G|s\"\"\"a >${x}<G| b\nc>"
        )
        // Scala 2 gives up highlighting at a single-line literal left unclosed on
        // a line that isn't the last one, Scala 3 resumes on the next line (fully
        // since 3.10, whose scanner recovers as if the line ended there)
        test("nextLine") - {
          val expected =
            if (isScala2) "<Y|val> x = \"a\nval y = 2"
            else if (isScala3_10Plus) "<Y|val> x = <G|\"a>\n<Y|val> y = <G|2>"
            else "<Y|val> x = <G|\"a>\nval y = <G|2>"
          check("val x = \"a\nval y = 2", expected)
        }
      }
      test("annotation") {
        test("args") - check(
          """@deprecated("a", "b") def f = 1""",
          """@<G|deprecated>(<G|"a">, <G|"b">) <Y|def> f = <G|1>"""
        )
        test("noArgs") - check(
          "@main def run() = ()",
          "@<G|main> <Y|def> run() = ()"
        )
        test("qualified") - {
          val expected =
            if (isScala2) "@<G|scala.transient> <Y|val> x = <G|1>"
            else "@scala.<G|transient> <Y|val> x = <G|1>"
          check("@scala.transient val x = 1", expected)
        }
      }
      test("underscore") - {
        val expected =
          if (isScala2) """<Y|val> <Y|_> = <G|1>"""
          else """<Y|val> _ = <G|1>"""
        check("""val _ = 1""", expected)
      }
      test("nonTrivial") - {
        val underscore =
          if (isScala2) "<Y|_>"
          else "_"
        val underscore0 =
          if (isScala2) "<Y|_>*"
          else if (isScala3_01) "_*"
          else "_<G|*>"
        def stringColl(collType: String) =
          if (isScala2) s"<G|$collType>[<G|String>]"
          else s"<G|$collType[String]>"
        check(
          """def processLine(stmts: Seq[String],
                            saveHistory: (String => Unit, String) => Unit,
                            printer: Iterator[String] => Unit) = for{
              _ <- Catching { case Ex(x@_*) =>
                val Res.Failure(trace) = Res.Failure(x)
                Res.Failure(trace + "\nSomething unexpected went wrong =(")
              }
              Preprocessor.Output(code, printSnippet) <- preprocess(stmts, eval.getCurrentLine)
              _ = saveHistory(history.append(_), stmts.mkString("; "))
              oldClassloader = Thread.currentThread().getContextClassLoader
              out <- try{
                Thread.currentThread().setContextClassLoader(eval.evalClassloader)
                eval.processLine(
                  code,
                  s"ReplBridge.combinePrints(${printSnippet.mkString(", ")})",
                  printer
                )
              } finally Thread.currentThread().setContextClassLoader(oldClassloader)
            } yield out
        """,
          s"""<Y|def> processLine(stmts: ${stringColl("Seq")},
                            saveHistory: (<G|String> => <G|Unit>, <G|String>) => <G|Unit>,
                            printer: ${stringColl("Iterator")} => <G|Unit>) = <Y|for>{
              $underscore <- Catching { <Y|case> Ex(x@$underscore0) =>
                <Y|val> Res.Failure(trace) = Res.Failure(x)
                Res.Failure(trace + <G|"\\nSomething unexpected went wrong =(">)
              }
              Preprocessor.Output(code, printSnippet) <- preprocess(stmts, eval.getCurrentLine)
              $underscore = saveHistory(history.append($underscore), stmts.mkString(<G|"; ">))
              oldClassloader = Thread.currentThread().getContextClassLoader
              out <- <Y|try>{
                Thread.currentThread().setContextClassLoader(eval.evalClassloader)
                eval.processLine(
                  code,
                  <G|s"ReplBridge.combinePrints(>$${printSnippet.mkString(<G|", ">)}<G|)">,
                  printer
                )
              } <Y|finally> Thread.currentThread().setContextClassLoader(oldClassloader)
            } <Y|yield> out
        """
        )
      }
    }
  }
}
