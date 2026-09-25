package ammonite.compiler.tools

import scala.language.implicitConversions

/**
 * The command used by [[source]] to display source code
 */
case class Strings(values: Seq[String])
object Strings {
  implicit def stringPrefix(s: String): Strings = Strings(Seq(s))
  implicit def stringSeqPrefix(s: Seq[String]): Strings = Strings(s)
}
