// a Scala 3 macro, used by the next block: the blocks of a script are compiled one after
// the other, under the path of the script
object Mac {
  import scala.quoted.*
  inline def show(inline x: Any): String = ${ impl('x) }
  def impl(x: Expr[Any])(using Quotes): Expr[String] = Expr(x.show)
}
@
val res = Mac.show(1 + 2)
