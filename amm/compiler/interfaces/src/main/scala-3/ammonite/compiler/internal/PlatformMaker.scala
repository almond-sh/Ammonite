package ammonite.compiler.internal

import java.net.URL

import dotty.tools.dotc.config.Platform
import dotty.tools.io.AbstractFile

/**
 * Creates the platform of the compilers Ammonite runs, which holds their class path.
 *
 * The class path API of dotty kept moving: `ClassPath#classes` hands back `BinaryFileEntry`s
 * rather than `ClassFileEntry`s since 3.4.0, `ClassPath#findClassFile` gave way to
 * `findClassFileAndModuleFile` in 3.9.0, and 3.10.0 moved `ClassPath` from `dotty.tools.io` to
 * `dotty.tools.dotc.classpath`, listing packages and classes by `String` rather than
 * `PackageName`. Only `Platform` itself stayed put, so it is all we hand back.
 */
trait PlatformMaker {

  /**
   * A platform whose class path lists, in that order, the white-listed parts of
   * `initialClassPath`, `classPath`, the JDK classes, and `dynamicClassPath`.
   *
   * @param initialClassPath the class path Ammonite itself runs with, of which only what
   *                         `whiteList` lists is visible from user code
   * @param classPath the class path of user code, which lists `initialClassPath` too
   * @param dynamicClassPath where the classes compiled from user code end up
   * @param whiteList the paths of the packages and classes of `initialClassPath` that are
   *                  visible from user code, like `Seq("scala", "Predef.class")`
   */
  def platform(
      initialClassPath: Seq[URL],
      classPath: Seq[URL],
      dynamicClassPath: AbstractFile,
      whiteList: Set[Seq[String]]
  ): Platform
}
