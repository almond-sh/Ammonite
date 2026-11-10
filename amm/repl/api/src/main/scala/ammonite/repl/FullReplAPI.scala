package ammonite.repl

import ammonite.interp.api.APIHolder
import ammonite.repl.api.ReplAPI
import ammonite.util._
import ammonite.util.Util.newLine

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.reflect.classTag

trait FullReplAPI extends ReplAPI with FullReplAPIScalaVersionSpecific { replApi =>

  protected[ammonite] val colors: Ref[Colors]

  def help =
    """Welcome to the Ammonite Scala REPL! Enter a Scala expression and it will be evaluated.
      |All your standard Bash hotkeys should work for navigating around or editing the line
      |being entered, as well as some GUI hotkeys like alt-shift-left/right to select words
      |to replace. Hit <tab> to autocomplete possible names.
      |
      |For a list of REPL built-ins and configuration, use `repl.<tab>`. For a more detailed
      |description of how to use the REPL, check out https://ammonite.io
    """.stripMargin.trim

  protected[this] def internal0: FullReplAPI.Internal =
    new FullReplAPI.Internal {
      def pprinter = replApi.pprinter
      def colors = replApi.colors
    }

  /**
   * This stuff is used for the REPL-generated code that prints things;
   * shouldn't really be used by users, but needs to be public and accessible
   */
  lazy val Internal: FullReplAPI.Internal = internal0
}

object FullReplAPI {

  /** Separates the field names passed to `Internal.printTyped` */
  final val printTypedFieldSeparator = "\u0000"

  /**
   * A `TPrint` for a type already rendered as a string.
   *
   * On Scala 3, the Ammonite compiler phase renders the types of the values the REPL prints
   * itself, and passes them to `Internal.printTyped`, that hands one of these to
   * `Internal.print`. For named tuples, `namedTupleFields` has the names of their fields.
   */
  final class RenderedType[T](val tpe: String, val namedTupleFields: List[String])
      extends pprint.TPrint[T] {
    def render(implicit tpc: pprint.TPrintColors): fansi.Str =
      colorType(tpe, tpc.typeColor)
  }

  /**
   * Colors the names in a type rendered as a string, leaving punctuation,
   * operators and literals uncolored, like pprint's `TPrint` does
   */
  def colorType(tpe: String, color: fansi.Attrs): fansi.Str = {
    var res = fansi.Str("")
    var i = 0
    while (i < tpe.length) {
      val c = tpe.charAt(i)
      val end =
        if (c == '`') {
          val idx = tpe.indexOf('`', i + 1)
          if (idx < 0) tpe.length else idx + 1
        } else if (c == '"') {
          var j = i + 1
          while (j < tpe.length && tpe.charAt(j) != '"') {
            if (tpe.charAt(j) == '\\') j += 1
            j += 1
          }
          math.min(j + 1, tpe.length)
        } else if (Character.isJavaIdentifierStart(c)) {
          var j = i + 1
          while (j < tpe.length && Character.isJavaIdentifierPart(tpe.charAt(j))) j += 1
          j
        } else i + 1
      val chunk = fansi.Str(tpe.substring(i, end), errorMode = fansi.ErrorMode.Strip)
      res = res ++ (if (c == '`' || Character.isJavaIdentifierStart(c)) color(chunk) else chunk)
      i = end
    }
    res
  }

  trait Internal {
    def pprinter: Ref[pprint.PPrinter]
    def colors: Ref[Colors]

    def combinePrints(iters: Iterator[String]*) = {
      iters.toIterator
        .filter(_.nonEmpty)
        .flatMap(Iterator(newLine) ++ _)
        .drop(1)
    }

    /**
     * Prints the value of a REPL result.
     *
     * Can be overridden by libraries using Ammonite, to change how results are printed.
     */
    def print[T: pprint.TPrint](value: => T, ident: String, custom: Option[String])(implicit
        tcolors: pprint.TPrintColors,
        classTagT: ClassTag[T] = null
    ): Iterator[String] = {
      // Here we use ClassTag to detect if T is an Unit.
      //
      // The default value null suppresses the compilation error when T is a singleton type,
      // which can't provide a ClassTag.
      //
      // We don't use `asUnit: T =:= Unit = null` because that approach does not work
      // when T is Nothing.
      // See https://github.com/scala/bug/issues/10393 for further information.
      //
      // We don't use WeakTypeTag or TypeTag because those type classes are too heavy-weight,
      // as Scalac will generate a huge amount of code for creating a TypeTag for refinement types.
      // See https://github.com/lihaoyi/Ammonite/issues/649 for further information.
      //
      // We do not check `value == ()`, because that would force evaluation of `value`, which
      // may be defined as a `lazy val` which the user explicitly does not want to evaluate
      val isUnit = classTagT == classTag[Unit]

      if (isUnit) Iterator()
      else {

        // Pre-compute how many lines and how many columns the prefix of the
        // printed output takes, so we can feed that information into the
        // pretty-printing of the main body
        val prefix = new pprint.Truncated(
          Iterator(
            colors().ident()(ident).render,
            ": ",
            implicitly[pprint.TPrint[T]].render(tcolors),
            " = "
          ),
          pprinter().defaultWidth,
          pprinter().defaultHeight
        )
        val output = mutable.Buffer.empty[fansi.Str]

        prefix.foreach(output += _)

        val rhs = custom match {
          case None =>
            val printer = pprinter()
            val namedTupleFields = implicitly[pprint.TPrint[T]] match {
              case r: FullReplAPI.RenderedType[_] => r.namedTupleFields
              case _ => Nil
            }
            val value0 = value
            val printer0 = value0 match {
              case p: Product if namedTupleFields.nonEmpty &&
                  p.productArity == namedTupleFields.length =>
                // Named tuples are plain tuples at runtime, so pprint can't know
                // their field names: we print them ourselves, like case class fields
                val handler: PartialFunction[Any, pprint.Tree] = {
                  case v if v.asInstanceOf[AnyRef] eq p.asInstanceOf[AnyRef] =>
                    pprint.Tree.Apply(
                      "",
                      namedTupleFields.iterator.zip(p.productIterator).map {
                        case (name, elem) =>
                          pprint.Tree.KeyValue(
                            name,
                            printer.treeify(
                              elem,
                              printer.defaultEscapeUnicode,
                              printer.defaultShowFieldNames
                            )
                          )
                      }
                    )
                }
                printer.copy(additionalHandlers = handler.orElse(printer.additionalHandlers))
              case _ => printer
            }
            printer0.tokenize(
              value0,
              height = printer0.defaultHeight - prefix.completedLineCount,
              initialOffset = prefix.lastLineLength
            ).toStream
          case Some(s) => Seq(pprinter().colorLiteral(s))
        }

        output.iterator.map(_.render) ++ rhs.map(_.render)
      }
    }

    /**
     * Prints the value of a REPL result, given its type rendered as a string.
     *
     * Called by the code the REPL generates on Scala 3, where the Ammonite compiler phase
     * replaces `tpe` (left empty in the generated code) by the type of `value` rendered by
     * the compiler, and `namedTupleFields` by the names of its fields (separated by
     * `printTypedFieldSeparator`) if it's a named tuple. This delegates to `print`.
     */
    def printTyped[T](
        value: => T,
        ident: String,
        custom: Option[String],
        tpe: String,
        namedTupleFields: String
    )(implicit
        tcolors: pprint.TPrintColors,
        classTagT: ClassTag[T] = null
    ): Iterator[String] =
      print(value, ident, custom)(renderedType(value, tpe, namedTupleFields), tcolors, classTagT)

    /**
     * The type of `value`, rendered by the compiler like for the values the REPL prints.
     *
     * On Scala 3, the Ammonite compiler phase fills `tpe` and `namedTupleFields`, when left
     * empty in the code it compiles, the way it does for `printTyped`. This lets libraries
     * using Ammonite get the same types as the ones the REPL prints, from the code they add
     * to the commands (pprint's `TPrint` renders some Scala 3 types badly, like named tuples).
     * `value` isn't evaluated.
     */
    def renderedType[T](
        value: => T,
        tpe: String,
        namedTupleFields: String
    ): FullReplAPI.RenderedType[T] = {
      val fieldNames =
        if (namedTupleFields.isEmpty) Nil
        else namedTupleFields.split(FullReplAPI.printTypedFieldSeparator, -1).toList
      new FullReplAPI.RenderedType[T](tpe, fieldNames)
    }

    def printDef(definitionLabel: String, ident: String) = {
      Iterator(
        "defined ",
        colors().`type`()(definitionLabel).render,
        " ",
        colors().ident()(ident).render
      )
    }
    def printImport(imported: String) = {
      Iterator(colors().`type`()("import ").render, colors().ident()(imported).render)
    }
    def printExport(exported: String) = {
      Iterator(colors().`type`()("export ").render, colors().ident()(exported).render)
    }
  }
}

object ReplBridge extends APIHolder[FullReplAPI]
