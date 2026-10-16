package ammonite.compiler.internal

import java.net.URL

import dotty.ammonite.compiler.UrlJarClassPath3_9_0
import dotty.tools.io.ClassPath

/** The [[UrlJarClassPathMaker]] for the compilers of 3.9.0 and later. */
class UrlJarClassPathMaker3_9_0 extends UrlJarClassPathMaker {
  def classPath(url: URL, root: UrlJarArchive.Dir): ClassPath =
    new UrlJarClassPath3_9_0(url, root)
}
