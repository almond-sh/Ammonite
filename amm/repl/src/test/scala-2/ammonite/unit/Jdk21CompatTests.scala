package ammonite.unit

import ammonite.compiler.Compiler
import utest._

object Jdk21CompatTests extends TestSuite {
  // Scala compilers older than these fail with "bad constant pool index" when they load the
  // JDK classes of Java 21 and later - Ammonite then tells which Scala version is needed
  val tests = Tests {
    test("tooOld") {
      assert(Compiler.jdk21MinScalaVersion("2.12.8") == Some("2.12.18"))
      assert(Compiler.jdk21MinScalaVersion("2.12.17") == Some("2.12.18"))
      assert(Compiler.jdk21MinScalaVersion("2.13.3") == Some("2.13.11"))
      assert(Compiler.jdk21MinScalaVersion("2.13.10") == Some("2.13.11"))
    }
    test("recentEnough") {
      assert(Compiler.jdk21MinScalaVersion("2.12.18") == None)
      assert(Compiler.jdk21MinScalaVersion("2.12.21") == None)
      assert(Compiler.jdk21MinScalaVersion("2.13.11") == None)
      assert(Compiler.jdk21MinScalaVersion("2.13.18") == None)
    }
  }
}
