package ammonite.compiler.internal

/**
 * Picks the implementations of the `ammonite.compiler.internal` interfaces that were written
 * for the Scala compiler `ammonite-compiler` finds on its class path.
 *
 * `ammonite-compiler` is published once per binary Scala version, but the compiler internals
 * it drives keep moving between patch releases. It therefore ships one implementation of each
 * of these interfaces per interval of Scala versions - `ReporterMaker2_13_12` covers 2.13.12
 * and later, `ReporterMaker2_12_13__2_13_11` covers 2.12.13 to 2.13.11 - each compiled against
 * the oldest compiler of its interval, and picks between them here.
 */
object InternalsLoader {

  /**
   * The version of the Scala compiler `compilerClass` comes from, like `2.13.18`.
   *
   * Read from the `compiler.properties` resource that both scalac and dotty ship their version
   * in, rather than from an API of theirs that could move between the versions we support.
   */
  def compilerVersion[T](compilerClass: Class[T]): String = {
    val stream = compilerClass.getResourceAsStream("/compiler.properties")
    if (stream == null)
      throw new RuntimeException(
        s"No compiler.properties alongside ${compilerClass.getName}, cannot tell which " +
          "version of the Scala compiler Ammonite is running with"
      )
    val properties = new java.util.Properties
    try properties.load(stream)
    finally stream.close()
    val version = properties.getProperty("version.number")
    if (version == null)
      throw new RuntimeException(
        s"No version.number in the compiler.properties alongside ${compilerClass.getName}"
      )
    version
  }

  /**
   * The implementation of `interface` written for `compilerVersion`.
   *
   * `variants` pairs the oldest Scala version an implementation covers with the suffix its
   * class name ends with, oldest first. A compiler older than any of them gets the oldest
   * implementation, which is the closest thing we have to one written for it.
   */
  def load[T](
      interface: Class[T],
      compilerVersion: String,
      variants: (String, String)*
  ): T = {
    val suffix = variants
      .takeWhile { case (since, _) => compare(since, compilerVersion) <= 0 }
      .lastOption
      .getOrElse(variants.head)
      ._2
    val className = interface.getName + suffix
    val cls =
      try interface.getClassLoader.loadClass(className)
      catch {
        case e: ClassNotFoundException =>
          throw new RuntimeException(
            s"$className, the ${interface.getSimpleName} of Ammonite for Scala " +
              s"$compilerVersion, is missing from the class path",
            e
          )
      }
    interface.cast(cls.getDeclaredConstructor().newInstance())
  }

  /** Compares two dotted version numbers, ordering `2.13.9` before `2.13.10`. */
  def compare(a: String, b: String): Int = {
    val aParts = parts(a)
    val bParts = parts(b)
    val diff = aParts
      .zipAll(bParts, 0, 0)
      .collectFirst { case (l, r) if l != r => l - r }
    diff.getOrElse(0)
  }

  private def parts(version: String): Seq[Int] =
    version
      .split("[.\\-]")
      .toSeq
      .map(part => if (part.forall(_.isDigit) && part.nonEmpty) part.toInt else 0)
}
