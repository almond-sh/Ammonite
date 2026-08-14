// Mill can be told to use another output directory than the default "out" one,
// via the MILL_OUTPUT_DIR environment variable
val outDir = sys.env.get("MILL_OUTPUT_DIR").filter(_.nonEmpty) match {
  case Some(dir) => os.Path(dir, os.pwd)
  case None => os.pwd / "out"
}
println(os.list(outDir))
