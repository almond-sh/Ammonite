// MultiMain.sc

val x = 1

@main
def mainA() = {
  println("Hello! " + x)
}

@main
def functionB(
    i: Int,
    s: String,
    path: java.nio.file.Path = java.nio.file.Paths.get("").toAbsolutePath
) = {
  println(s"Hello! ${s * i} ${java.nio.file.Paths.get("").toAbsolutePath.relativize(path)}.")
}