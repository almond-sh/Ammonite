// an experimental language import, that applies to the next blocks too
import scala.language.experimental.saferExceptions
def f(n: Int): Int throws Exception = if (n > 0) n else throw new Exception("neg")
@
val res = try f(-1) catch { case _: Exception => 0 }
