package ammonite.compiler.internal

import java.net.URL

import dotty.tools.io.ClassPath

/**
 * Creates the class path of the JARs Ammonite can only reach through a URL, like those that
 * coursier standalone launchers embed, that dotty doesn't support itself.
 *
 * `ClassPath#classes` hands back `BinaryFileEntry`s rather than `ClassFileEntry`s since 3.4.0,
 * and `ClassPath#findClassFile` gave way to `findClassFileAndModuleFile` in 3.9.0.
 */
trait UrlJarClassPathMaker {

  /** The class path of the JAR at `url`, whose content is `root`. */
  def classPath(url: URL, root: UrlJarArchive.Dir): ClassPath
}
