// ObjectsOnly.sc
// Only object definitions: with --class-based, these get wrapped in a singleton
// rather than in a class

object Greeter {
  def greet(n: Int) = Seq.fill(n)("Hello").mkString(" ")
}
