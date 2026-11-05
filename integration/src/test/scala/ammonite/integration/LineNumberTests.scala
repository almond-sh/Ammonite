package ammonite.integration
import ammonite.util.Util
import utest._
import TestUtils._

/**
 * Mostly already tested in the `ammonite.main` unit tests; one test case is
 * left here just to verify end-to-end correctness
 */
object LineNumberTests extends TestSuite {
  val tests = this {

    def checkErrorMessage(file: os.RelPath, expected: String): Unit = {
      val e = assertThrows[os.SubprocessException] {
        exec(file)
      }.result.err.text()
      assert(TestUtils.containsLines(e, expected))
    }

    // compared numerically, as "3.10.0" < "3.3.4" as strings
    val isPre3_3_4 = {
      val Array(major, minor, patch) =
        scalaVersion.split('.').take(3).map(_.takeWhile(_.isDigit).toInt)
      major < 3 || (major == 3 && (minor < 3 || (minor == 3 && patch < 4)))
    }

    test("compilationErrorInSecondBlock") {
      val path = os.rel / "lineNumbers" / "compilationErrorInSecondBlock.sc"
      val sp = " "
      checkErrorMessage(
        file = path,
        expected = Util.normalizeNewlines(
          if (isScala2)
            """compilationErrorInSecondBlock.sc:14: not found: value printnl
              |val res_0 = printnl("OK")
              |            ^""".stripMargin
          else if (isPre3_3_4)
            s"""-- [E006] Not Found Error: ${replStandaloneResources / path}:1:12$sp
               |1 |val res_0 = printnl("OK")
               |  |            ^^^^^^^
               |  |            Not found: printnl
               |Compilation Failed""".stripMargin
          else
            s"""-- [E006] Not Found Error: ${replStandaloneResources / path}:1:12$sp
               |1 |val res_0 = printnl("OK")
               |  |            ^^^^^^^
               |  |    Not found: printnl - did you mean print? or perhaps printf or println?
               |  |
               |  | longer explanation available when compiling with `-explain`
               |Compilation Failed""".stripMargin
        )
      )
    }

  }
}
