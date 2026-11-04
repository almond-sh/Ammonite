package ammonite.integration
object TestMain {
  def main(args: Array[String]): Unit = {
    // Break into debug REPL with
    ammonite.Main(
      predefCode = "println(\"Starting Debugging!\")"
    ).run()
  }
}
