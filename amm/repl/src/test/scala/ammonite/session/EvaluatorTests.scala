package ammonite.session

import ammonite.DualTestRepl
import ammonite.TestUtils._
import utest._

import scala.collection.{immutable => imm}

object EvaluatorTests extends TestSuite {

  val tests = Tests {
    println("EvaluatorTests")
    val check = new DualTestRepl()
    test("simpleExpressions") {
      check.session("""
        @ 1 + 2
        res0: Int = 3

        @ res0
        res1: Int = 3

        @ res0 + res1
        res2: Int = 6
      """)
    }
    test("vals") {
      check.session("""
        @ val x = 10L
        x: Long = 10L

        @ x
        res1: Long = 10L

        @ val y = x + 1
        y: Long = 11L

        @ x * y
        res3: Long = 110L

        @ val `class` = "class"
        `class`: String = "class"

        @ `class`
        res5: String = "class"
      """)
    }
    test("lazyvals") {
      // It actually appears when I ask for it, and the
      // actual evaluation happens in the correct order
      check.session("""
        @ lazy val x = 'h'
        x: Char = <lazy>

        @ x
        res1: Char = 'h'

        @ var w = 'l'
        w: Char = 'l'

        @ lazy val y = {w = 'a'; 'A'}
        y: Char = <lazy>

        @ lazy val z = {w = 'b'; 'B'}
        z: Char = <lazy>

        @ z
        res5: Char = 'B'

        @ y
        res6: Char = 'A'

        @ w
        res7: Char = 'a'
      """)
    }

    test("vars") {
      check.session("""
        @ var x: Int = 10
        x: Int = 10

        @ x
        res1: Int = 10

        @ x = 1

        @ x
        res3: Int = 1
      """)
    }

    test("defs") {
      check.session("""
        @ def sumItAll[T: Numeric](i: Seq[T]): T = {i.sum}
        defined function sumItAll

        @ sumItAll(Seq(1, 2, 3, 4, 5))
        res1: Int = 15

        @ sumItAll(Seq(1L, 2L, 3L, 4L, 5L))
        res2: Long = 15L
      """)
    }
    test("types") {
      check.session("""
        @ type Funky = Array[Array[String]]
        defined type Funky

        @ val arr: Funky = Array(Array("Hello!"))
        arr: Funky = Array(Array("Hello!"))

        @ type Funky2[T] = Array[Array[T]]
        defined type Funky2

        @ val arr: Funky2[Int] = Array(Array(123))
        arr: Funky2[Int] = Array(Array(123))
      """)
    }
    test("scala3Types") {
      // On Scala 3, result types are rendered by the compiler (see ResultTypes in amm.compiler)
      if (check.scala2) "N/A"
      else check.session("""
        @ val u: Int | String = 2
        u: Int | String = 2

        @ val i: Comparable[String] & CharSequence = "x"
        i: Comparable[String] & CharSequence = "x"

        @ val s: "lit" = "lit"
        s: "lit" = "lit"

        @ object O { opaque type Id = Int; def mk(i: Int): Id = i }
        defined object O

        @ O.mk(2)
        res4: O.Id = 2

        @ type M[X] = X match { case Int => String; case String => Int }
        defined type M

        @ def m[X](x: X): M[X] = ???
        defined function m

        @ lazy val mi = m(2)
        mi: String = <lazy>

        @ type TL = [X] =>> Map[X, X]
        defined type TL

        @ val tl: TL[Int] = Map(1 -> 2)
        tl: TL[Int] = Map(1 -> 2)

        @ class Foo { class Bar { override def toString = "bar" }; val b = new Bar }
        defined class Foo

        @ val foo = new Foo
        foo: Foo = ?

        @ foo.b
        res12: foo.Bar = bar

        @ object Outer { case class In(i: Int); object Deep { case class D() } }
        defined object Outer

        @ Outer.In(2)
        res14: Outer.In = In(i = 2)

        @ Outer.Deep.D()
        res15: Outer.Deep.D = D()

        @ given Int = 2
        given_Int: Int = <given>

        @ extension (i: Int) def twice = (i * 2, List(i))
        defined extension method twice

        @ 2.twice
        res18: (Int, List[Int]) = (4, List(2))
      """)
    }

    test("namedTuples") {
      // named tuples are standard since Scala 3.7
      if (check.scala2 || check.scalaVersion.startsWith("3.3.")) "N/A"
      else check.session("""
        @ val t = (name = "a", age = 2)
        t: (name : String, age : Int) = (name = "a", age = 2)

        @ val single = (x = 1)
        single: (x : Int) = (x = 1)

        @ List((a = 1, b = "x"))
        res2: List[(a : Int, b : String)] = List((1, "x"))
      """)
    }

    test("renderedType") {
      // Internal.renderedType gives the types the REPL prints, for libraries using Ammonite
      if (check.scala2) "N/A"
      else {
        check.session("""
          @ val l = List(1, 2)
          l: List[Int] = List(1, 2)

          @ val u: Int | String = 2
          u: Int | String = 2

          @ repl.Internal.renderedType(l, "", "").tpe
          res2: String = "List[Int]"

          @ repl.Internal.renderedType(u, "", "").tpe
          res3: String = "Int | String"

          @ repl.Internal.renderedType(u, "", "").namedTupleFields
          res4: List[String] = List()
        """)
        if (!check.scalaVersion.startsWith("3.3."))
          check.session("""
            @ val t = (name = "a", age = 2)
            t: (name : String, age : Int) = (name = "a", age = 2)

            @ repl.Internal.renderedType(t, "", "").tpe
            res6: String = "(name : String, age : Int)"

            @ repl.Internal.renderedType(t, "", "").namedTupleFields
            res7: List[String] = List("name", "age")
          """)
      }
    }

    test("library") {
      // x and y pprinted value is test("non") - empty iterator' up to 2.12.6,
      // '<iterator>' since 2.12.7, hence the '?' (don't check the value)
      check.session("""
        @ val x = Iterator.continually(1)
        x: Iterator[Int] = ?

        @ val y = x.take(15)
        y: Iterator[Int] = ?

        @ val z = y.foldLeft(0)(_ + _)
        z: Int = 15
      """)
    }

    test("classes") {
      check.session("""
        @ class C{override def toString() = "Ceee"}
        defined class C

        @ new C
        res1: C = Ceee

        @ case object CO
        defined object CO

        @ CO
        res3: CO.type = CO

        @ case class CC()
        defined class CC

        @ CC()
        res5: CC = CC()

        @ CO
        res6: CO.type = CO

        @ case class CO()
        defined class CO

        @ CO // res8: CO.type = CO

        @ CO()
        res9: CO = CO()

        @ CO == res3
        res10: Boolean = false
      """)

      // there used to be a
      //     @ CO
      //     res8: CO.type = CO
      // that fails with class wrapping in 2.12 since #853, giving instead
      //     res8: CO = CO
      // CO != res3 should test roughly the same thing
    }

    test("packageImport") {
      check.session("""
        @ import java.util._

        @ import concurrent.atomic._
      """)
    }

    test("nesting") {
      check.session("""
        @ val x = 1
        x: Int = 1

        @ val x = 2
        x: Int = 2

        @ x
        res2: Int = 2

        @ object X{ val Y = 1 }
        defined object X

        @ object X{ val Y = 2 }
        defined object X

        @ X.Y
        res5: Int = 2
      """)
    }
    test("nestedImports") {
      check.session("""
        @ class Foo { object values { def a = "a" }; override def toString = "Foo" }
        defined class Foo

        @ val foo = new Foo; import foo.values._
        foo: Foo = Foo
        import foo.values._

        @ val a0 = a
        a0: String = "a"
      """)
    }
    test("multistatement") {
      check.session(s"""
        @ ;1; 2L; '3';
        res0_0: Int = 1
        res0_1: Long = 2L
        res0_2: Char = '3'

        @ val x = 1; x;
        x: Int = 1
        res1_1: Int = 1

        @ var x = 1; x = 2; x
        x: Int = 2
        res2_2: Int = 2

        @ var y = 1; case class C(i: Int = 0){ def foo = x + y }; new C().foo
        y: Int = 1
        defined class C
        res3_2: Int = 3

        @ C()
        res4: C = ${Print.C(i = 0)}
      """)
    }

    test("multiassign") {
      check.session("""
        @ val (a, b) = (1, 2)
        a: Int = 1
        b: Int = 2

        @ a
        res1: Int = 1

        @ val (a, b) = (1, 2); val (c, d) = (3, 4)
        a: Int = 1
        b: Int = 2
        c: Int = 3
        d: Int = 4
      """)
    }
    test("parsingProblems") {
      check.session("""
        @ (1 + 1)
        res0: Int = 2

        @ (
        @ 1
        @ +
        @ 1
        @ )
        res1: Int = 2

        @ (
        @ (123.0).round
        @        .toChar
        @ )
        res2: Char = '{'

        @ Seq(0) map {_ + 1}
        res3: Seq[Int] = List(1)
      """)
    }
    test("backticks") {
      check.session("""
        @ val `1+1` = 1
        `1+1`: Int = 1

        @ val ++ = 1
        ++: Int = 1

        @ object `+1+`
        defined object `+1+`

        @ val ` ` = 1; type ` ` = Int
        ` `: Int = 1
        defined type ` `

        @ ((` ` + `1+1`): ` `): Int
        res4: Int = 2

        @ object ` `{ val ` ` = 333 }

        @ import ` `.` `

        @ ` `
        res7: Int = 333

        @ object ` `{ val ` ` = 123};
        defined object ` `

        @ import ` `.{` ` => `l o l`}

        @ `l o l`
        res10: Int = 123

        @ import ` `.{` ` => `//`}
        
        @ `//`
        res12: Int = 123

        @ import ` `.{` ` => `Something then //`}

        @ `Something then //`
        res14: Int = 123

        @ import ` `.{` ` => `// and then`}

        @ `// and then`
        res16: Int = 123

        @ import ` `.{` ` => `/*`}

        @ `/*`
        res18: Int = 123

        @ import ` `.{` ` => `/*/`}

        @ `/*/`
        res20: Int = 123

        @ import ` `.{` ` => `*/`}

        @ `*/`
        res22: Int = 123

        @ val `/*` = 123
        `/*`: Int = 123

        @ val `//` = 123
        `//`: Int = 123

      """)
    }
    test("referenceTraitFromPreviousCommand") {
      // When class-based wrapping is enabled, the second command references a trait from the
      // first one, and this case has to be handled by the "used earlier definitions" mechanism.
      check.session("""
        @ trait A; class X(x: Int) extends A { override def toString = s"X($x)" }
        defined trait A
        defined class X

        @ class Z[T <: A](t: T) { override def toString = s"Z($t)" }; val z1 = new Z(new X(7))
        defined class Z
        z1: Z[X] = Z(X(7))

        @ val z2 = new Z(new X(7))
        z2: Z[X] = Z(X(7))
      """)
    }
  }
}
