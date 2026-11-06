// MultiBlockMain.sc
val greeting = "Hello"
@
val punctuation = "!"
@
@main
def main(name: String, n: Int = 1) = {
  println(Seq.fill(n)(greeting + " " + name + punctuation).mkString(" "))
}
