// Args.sc
val x = 1

@main
def main(i: Int, s: String, path: java.nio.file.Path = os.pwd.toNIO): Unit = {
  println(s"Hello! ${s * i} ${path.getFileName}.")
}