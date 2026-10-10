package ammonite.main

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import utest._

object SessionDirectoryTests extends TestSuite {

  def run(args: String*): (Boolean, String) = {
    val err = new ByteArrayOutputStream
    val res = ammonite.AmmoniteMain.main0(
      args.toList,
      new ByteArrayInputStream(Array.emptyByteArray),
      new ByteArrayOutputStream,
      err
    )
    (res, new String(err.toByteArray))
  }

  val tests = Tests {
    test("bspRequiresSessionDirectory") {
      val (res, err) = run("--bsp-socket", "bsp.sock")
      assert(!res)
      assert(err.contains("--bsp-socket requires --session-directory"))
    }
    test("semanticDbRequiresSessionDirectory") {
      val (res, err) = run("--semanticdb")
      assert(!res)
      assert(err.contains("--semanticdb requires --session-directory"))
    }
    test("replOnly") {
      val (res, err) = run("--session-directory", "session", "--semanticdb", "-c", "1")
      assert(!res)
      assert(err.contains("--semanticdb is only supported when running the REPL"))
    }
  }
}
