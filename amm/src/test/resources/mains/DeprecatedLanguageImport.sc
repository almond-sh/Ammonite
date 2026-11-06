// namedTuples is standard since Scala 3.7, and Scala 3.10 warns about its import,
// which is carried over to the next blocks
import scala.language.experimental.namedTuples
val t = (a = 1, b = "x")
@
val b = t.b
@
println(s"Hello ${t.a} $b")
