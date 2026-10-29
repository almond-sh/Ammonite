// Mill can be told to use another output directory than the default "out" one,
// via the MILL_OUTPUT_DIR environment variable
val pwd = java.nio.file.Paths.get("").toAbsolutePath
val outDir = sys.env.get("MILL_OUTPUT_DIR").filter(_.nonEmpty) match {
  case Some(dir) => pwd.resolve(dir)
  case None => pwd.resolve("out")
}
val outDirContent = java.nio.file.Files.list(outDir)
try println(outDirContent.toArray.toSeq)
finally outDirContent.close()
