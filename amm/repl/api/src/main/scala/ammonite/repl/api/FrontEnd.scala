package ammonite.repl.api

import java.io.{InputStream, OutputStream}

import ammonite.util.{Colors, Res}

/** JLine interface */
trait FrontEnd {
  def width: Int
  def height: Int
  def action(
      input: InputStream,
      reader: java.io.Reader,
      output: OutputStream,
      prompt: String,
      colors: Colors,
      compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
      history: IndexedSeq[String],
      addHistory: String => Unit
  ): Res[(String, Seq[String])]

  /**
   * Reads a single line, like a command of the debugger prompt (rather than Scala code)
   *
   * Returns `None` upon end of input (Ctrl-D, say).
   */
  def readCommand(
      prompt: String,
      input: InputStream,
      reader: java.io.Reader,
      output: OutputStream,
      colors: Colors,
      history: IndexedSeq[String]
  ): Option[String] = {
    output.write(prompt.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    output.flush()
    val b = new StringBuilder
    var c = reader.read()
    while (c >= 0 && c != '\n') {
      if (c != '\r') b.append(c.toChar)
      c = reader.read()
    }
    if (c < 0 && b.isEmpty) None
    else Some(b.toString)
  }
}
