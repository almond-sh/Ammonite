package ammonite.util

/**
 * What Ammonite leaves of the imports that import hooks handle, in the code it compiles
 *
 * `import $ivy.`org::name:1.0`` or `import $file.foo` become `import $ivy.$` or
 * `import $file.$` (padded with spaces, to keep the positions of the rest of the code). Imports
 * that only import placeholders aren't printed, and placeholders are left out of the other ones.
 */
object ImportHookPlaceholder {

  /** The placeholder of an import hook, from its prefix, like `$ivy` */
  def apply(prefix: String): String = prefix + ".$"

  /** Whether `selector` (like `$ivy.$` or `a.b` in `import $ivy.$, a.b`) is a placeholder */
  def is(selector: String): Boolean = {
    val trimmed = selector.trim
    trimmed.length > 3 &&
    trimmed.startsWith("$") &&
    trimmed.endsWith(".$") &&
    trimmed.iterator.slice(1, trimmed.length - 2).forall(c => c != '.' && !c.isWhitespace)
  }

  /**
   * The text of an import, like `a.b, c.{d, e}` in `import a.b, c.{d, e}`, without the
   * placeholders of import hooks, or `None` if only these are left
   */
  def strip(imported: String): Option[String] = {
    val selectors = split(imported)
    if (selectors.exists(is)) {
      val kept = selectors.filterNot(is).map(_.trim)
      if (kept.isEmpty) None
      else Some(kept.mkString(", "))
    } else
      Some(imported)
  }

  // Splits at the commas separating the imported expressions, not those between braces
  private def split(imported: String): Seq[String] = {
    val selectors = Seq.newBuilder[String]
    var depth = 0
    var inBackticks = false
    var start = 0
    for (i <- 0 until imported.length)
      imported.charAt(i) match {
        case '`' => inBackticks = !inBackticks
        case '{' if !inBackticks => depth += 1
        case '}' if !inBackticks => depth -= 1
        case ',' if !inBackticks && depth == 0 =>
          selectors += imported.substring(start, i)
          start = i + 1
        case _ =>
      }
    selectors += imported.substring(start)
    selectors.result()
  }
}
