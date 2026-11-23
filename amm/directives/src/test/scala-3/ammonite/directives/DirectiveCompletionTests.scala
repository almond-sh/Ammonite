package ammonite.directives

import ammonite.directives.DirectiveCompletion.{Context, Key, Value}
import org.junit.Test
import org.junit.Assert.*

class DirectiveCompletionTests {

  // the cursor position is the one of the '|' in code, that is removed
  private def find(code: String): Option[Context] = {
    val pos = code.indexOf('|')
    assertTrue(pos >= 0)
    DirectiveCompletion.find(code.take(pos) + code.drop(pos + 1), pos)
  }

  @Test def quoted(): Unit = {
    assertEquals(
      Some(Value("dep", 15, "org.typelevel::cats-co")),
      find("""//> using dep "org.typelevel::cats-co|""")
    )
    assertEquals(
      Some(Value("dep", 15, "org.typelevel::cats-co")),
      find("""//> using dep "org.typelevel::cats-co|"""")
    )
  }

  @Test def bare(): Unit =
    assertEquals(
      Some(Value("dep", 14, "org.typelevel::cats-core:2.1")),
      find("//> using dep org.typelevel::cats-core:2.1|")
    )

  @Test def cursor_in_the_middle_of_a_value(): Unit =
    assertEquals(
      Some(Value("dep", 14, "org.typelevel::cats")),
      find("//> using dep org.typelevel::cats|-core:2.12.0")
    )

  @Test def other_keys(): Unit = {
    for (key <- Seq("deps", "script", "option", "repository"))
      assertEquals(Some(Value(key, 11 + key.length, "abc")), find(s"//> using $key abc|"))
  }

  @Test def several_values(): Unit = {
    assertEquals(
      Some(Value("deps", 46, "org.typelevel::cats-co")),
      find("""//> using deps "com.lihaoyi::os-lib:0.11.4", "org.typelevel::cats-co|""")
    )
    assertEquals(
      Some(Value("deps", 43, "org.typelevel::cats-co")),
      find("//> using deps com.lihaoyi::os-lib:0.11.4, org.typelevel::cats-co|")
    )
  }

  @Test def not_in_a_value_yet(): Unit = {
    assertEquals(Some(Value("dep", 14, "")), find("//> using dep |"))
    assertEquals(
      Some(Value("dep", 42, "")),
      find("""//> using dep "com.lihaoyi::os-lib:0.11.4"|""")
    )
  }

  @Test def later_directive_line(): Unit = {
    val code =
      """// a comment
        |//> using option -deprecation
        |
        |//> using script foo/ba|
        |
        |val n = 2
        |""".stripMargin
    assertEquals(Some(Value("script", code.indexOf("foo/"), "foo/ba")), find(code))
  }

  @Test def extra_spaces(): Unit =
    assertEquals(
      Some(Value("dep", 20, "org.typelevel::cats-co")),
      find("//>   using   dep   org.typelevel::cats-co|")
    )

  @Test def key(): Unit = {
    assertEquals(Some(Key(10, "de")), find("//> using de|"))
    assertEquals(Some(Key(10, "")), find("//> using |"))
    assertEquals(Some(Key(12, "scr")), find("//>  using  scr|"))
    assertEquals(None, find("//> using|"))
    assertEquals(None, find("//> usi|"))
  }

  @Test def after_code(): Unit = {
    val code =
      """val n = 2
        |//> using dep org.typelevel::cats-co|
        |""".stripMargin
    assertEquals(None, find(code))
  }

  @Test def code(): Unit = {
    assertEquals(None, find("val n = Lis|"))
    assertEquals(None, find("//> using dep org.typelevel::cats-core:2.12.0\nval n = Lis|"))
    assertEquals(None, find("// using dep org.typelevel::cats-co|"))
  }
}
