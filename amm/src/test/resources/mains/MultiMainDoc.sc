// MultiMainDoc.sc

val x = 1

@main
def mainA() = {
  println("Hello! " + x)
}

@arg(doc = "This explains what the function does")
@main
def functionB(@arg(doc =
                "how many times to repeat the string to make " +
                "it very very long, more than it originally was")
              i: Int ,
              @arg(doc = "the string to repeat")
              s: String ,
              path: java.nio.file.Path = os.pwd.toNIO) = {
  println(s"Hello! ${s * i} ${os.pwd.toNIO.relativize(path)}.")
}