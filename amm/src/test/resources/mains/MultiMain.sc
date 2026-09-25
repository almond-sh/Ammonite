// MultiMain.sc

val x = 1

@main
def mainA() = {
  println("Hello! " + x)
}

@main
def functionB(i: Int, s: String, path: java.nio.file.Path = os.pwd.toNIO) = {
  println(s"Hello! ${s * i} ${os.pwd.toNIO.relativize(path)}.")
}