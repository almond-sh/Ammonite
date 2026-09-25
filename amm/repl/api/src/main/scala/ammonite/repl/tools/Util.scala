package ammonite.repl.tools

import java.nio.file.{Path, Paths}

object Util {

  /**
   * Additional [[mainargs.TokensReader]] instance to teach it how to read paths, resolved
   * against the current directory when they are relative
   */
  implicit object PathRead extends mainargs.TokensReader.Simple[Path] {
    def shortName = "path"
    def read(strs: Seq[String]) = Right(Paths.get(strs.last).toAbsolutePath.normalize())
  }

}
