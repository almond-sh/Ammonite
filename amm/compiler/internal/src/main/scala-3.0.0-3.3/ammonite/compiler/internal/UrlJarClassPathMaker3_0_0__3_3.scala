package ammonite.compiler.internal

import java.net.URL

import dotty.ammonite.compiler.UrlJarClassPath3_0_0__3_3
import dotty.tools.io.ClassPath

/** The [[UrlJarClassPathMaker]] for the compilers up to 3.3.x. */
class UrlJarClassPathMaker3_0_0__3_3 extends UrlJarClassPathMaker {
  def classPath(url: URL, root: UrlJarArchive.Dir): ClassPath =
    new UrlJarClassPath3_0_0__3_3(url, root)
}
