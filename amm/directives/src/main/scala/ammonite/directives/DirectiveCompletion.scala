package ammonite.directives

/**
 * Finds what a cursor is in, in the using directives of some code, to complete them
 *
 * The compiler only sees directives as comments, so tools completing code look for the
 * directive key or value at the cursor here first, and complete it themselves.
 */
object DirectiveCompletion {

  /** What the cursor is in, in a directive */
  sealed abstract class Context extends Product with Serializable {

    /** Index in the code of the first character of the key or value being typed */
    def start: Int

    /** Key or value being typed, up to the cursor (empty if it's not started yet) */
    def prefix: String
  }

  /** The key of a directive, like the `de` of `//> using de` */
  final case class Key(start: Int, prefix: String) extends Context

  /**
   * The value of a directive, like the `org.typelevel::cats` of
   * `//> using dep org.typelevel::cats`
   *
   * @param start index of the first character of the value in the code (after its
   *              opening quote, if any)
   */
  final case class Value(key: String, start: Int, prefix: String) extends Context

  private val usingRegex = """//>\s*using(\s+|$)""".r
  private val keyRegex = """[^\s"`]*""".r

  /**
   * Finds the directive key or value the cursor is in, if any
   *
   * Only the directives of the preamble of the code, before any code, are considered, like
   * when running it.
   */
  def find(code: String, pos: Int): Option[Context] =
    if (pos < 0 || pos > code.length || !code.contains("//>")) None
    else
      CommentExtractor.extract(code.toIndexedSeq)
        .directiveLines
        .iterator
        .map(_.lineStartOffset) // offset of the "//>" of the line in the code
        .find { start =>
          val nl = code.indexOf('\n', start)
          val lineEnd = if (nl < 0) code.length else nl
          start <= pos && pos <= lineEnd
        }
        .flatMap { start =>
          context(code.substring(start, pos)).map {
            case Key(relStart, prefix) => Key(start + relStart, prefix)
            case Value(key, relStart, prefix) => Value(key, start + relStart, prefix)
          }
        }

  /**
   * Finds what's being typed at the end of a directive line, cut at the cursor
   *
   * @param line content of the directive line, from its `//>` up to the cursor
   * @return what's being typed, with an index relative to the beginning of `line`
   */
  private def context(line: String): Option[Context] =
    usingRegex.findPrefixMatchOf(line).flatMap { usingMatch =>
      if (usingMatch.group(1).isEmpty)
        // right after "//> using", no space typed yet
        None
      else {
        val keyStart = usingMatch.end
        val key = keyRegex.findPrefixOf(line.substring(keyStart)).getOrElse("")
        val keyEnd = keyStart + key.length
        if (keyEnd == line.length)
          Some(Key(keyStart, key))
        else if (!Character.isWhitespace(line.charAt(keyEnd)))
          // a quote or a backtick right after the key, that we can't help with
          None
        else
          Some(valueContext(key, line, keyEnd))
      }
    }

  private def valueContext(key: String, line: String, from: Int): Value = {
    // Same rules as Lexer, without escape sequences in quoted values (paths or dependencies
    // don't need them), so that indices in values match those in the code
    var idx = from
    var valueStart = idx
    var inValue = false
    var inQuotes = false
    while (idx < line.length) {
      val c = line.charAt(idx)
      if (inQuotes) {
        if (c == '"') {
          inQuotes = false
          inValue = false
        }
      } else if (c == '"') {
        inQuotes = true
        inValue = true
        valueStart = idx + 1
      } else if (Character.isWhitespace(c))
        inValue = false
      else if (c == ',' && (idx + 1 >= line.length || Character.isWhitespace(line.charAt(idx + 1))))
        inValue = false
      else if (!inValue) {
        inValue = true
        valueStart = idx
      }
      idx += 1
    }
    if (inValue) Value(key, valueStart, line.substring(valueStart))
    else Value(key, line.length, "")
  }
}
