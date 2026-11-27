package ammonite.debugger

import utest._

object SessionDebuggerTests extends TestSuite {

  val tests = Tests {

    test("inputFrameName") {
      // frame names from the debug adapter, and how they should be shown
      val expected = Seq(
        // Scala 3, decoded by the debug adapter
        "cmd2.Helper.<anon fun>(i: Int): Unit" -> "<anon fun>(i: Int): Unit",
        "cmd2.Helper.f(x: Int): Int" -> "f(x: Int): Int",
        "cmd2.Helper.<init>(): Unit" -> "cmd2",
        "cmd2.f(x: Int): Int" -> "f(x: Int): Int",
        // Scala 2, JVM names
        "cmd2$Helper.f(int): int" -> "f(int): int",
        "cmd2$Helper.$anonfun$r$1(cmd2$Helper,int): int" -> "<anon fun>(int): int",
        "cmd2$Helper.$anonfun$new$1(int): void" -> "<anon fun>(int): void",
        "cmd2$Helper.<init>(cmd2): void" -> "cmd2",
        // object wrappers (the default of the Ammonite REPL)
        "cmd2$.f(int): int" -> "f(int): int",
        "cmd2$.<clinit>(): void" -> "cmd2",
        // not from that input
        "cmd12$Helper.f(int): int" -> "cmd12$Helper.f(int): int",
        "Range.foreach(Function1): void" -> "Range.foreach(Function1): void"
      )
      for ((name, expected0) <- expected) {
        val obtained = SessionDebugger.inputFrameName(name, "cmd2")
        assert(obtained == expected0)
      }
    }

    test("jarSource") {
      val uri =
        "jar:file:///home/user/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/" +
          "scala-library/3.10.1/scala-library-3.10.1-sources.jar!/scala/collection/immutable/Range.scala"
      assert(
        SessionDebugger.jarSource(uri) ==
          Some(("scala-library-3.10.1-sources.jar", "scala/collection/immutable/Range.scala"))
      )
      assert(
        SessionDebugger.jarSource("jar:file:///a/b/debug-core-0.34.0%2B44-sources.jar!/a/B.java") ==
          Some(("debug-core-0.34.0+44-sources.jar", "a/B.java"))
      )
      assert(SessionDebugger.jarSource("/tmp/foo/Bar.scala").isEmpty)
    }

    test("displayValue") {
      // values and types of variables from the debug adapter, and how they should be shown
      val expected = Seq(
        ("3", "int") -> "3",
        ("2.5", "double") -> "2.5",
        ("true", "boolean") -> "true",
        ("c", "char") -> "'c'",
        ("\"Ada Lovelace\"", "String") -> "\"Ada Lovelace\"",
        ("\"a@1 \"b\"\"", "String") -> "\"a@1 \"b\"\"",
        ("String[2]@9283", "String[]") -> "String[2]",
        ("int[0]@12", "int[]") -> "int[0]",
        ("cmd2$Helper$P@9285 \"P(1,2.5)\"", "cmd2$Helper$P") -> "P(1,2.5)",
        ("$colon$colon@9284 \"List(1, 2)\"", "$colon$colon") -> "List(1, 2)",
        ("Foo@9286 \"a\nb\"", "Foo") -> "a\nb",
        ("Foo@9287", "Foo") -> "Foo@9287",
        ("null", "String") -> "null"
      )
      for (((value, tpe), expected0) <- expected) {
        val obtained = SessionDebugger.displayValue(value, Some(tpe))
        assert(obtained == expected0)
      }
    }

    test("errorMessage") {
      assert(SessionDebugger.errorMessage(ujson.Obj("success" -> true)).isEmpty)
      assert(
        SessionDebugger.errorMessage(ujson.Obj("success" -> false, "message" -> "nope")) ==
          Some("nope")
      )
    }
  }
}
