package ammonite.repl.api

/**
 * The debugger of the REPL, enabled when Ammonite is started with `--debugger`
 *
 * When the code of a command stops at a breakpoint, or upon a call to [[breakpoint]], the REPL
 * shows a debugger prompt, `(debug)`, that accepts gdb-like commands (`bt`, `info locals`,
 * `p <expr>`, `next`, `step`, `finish`, `continue`, … - type `help` there to list them).
 */
trait DebuggerAPI {

  /** Whether the debugger is enabled */
  def enabled: Boolean

  /** Stops the current thread in the debugger, like a breakpoint at the place of the call */
  def breakpoint(): Unit = DebuggerAPI.breakpoint()

  /**
   * Sets a breakpoint
   *
   * @param location where to set the breakpoint:
   *                 - `cmd3.sc:4` or `cmd3:4`: line 4 of the command whose wrapper is `cmd3`
   *                   (the number of the command in the REPL), even if it isn't run yet,
   *                 - `4`: line 4 of the latest command (the one calling this method, if
   *                   called from a command),
   *                 - `some.pkg.Foo:12`: line 12 of the class `some.pkg.Foo`, from a library,
   *                 - `/path/to/Foo.scala:12`: line 12 of a source file.
   * @return the number of the breakpoint
   */
  def break(location: String): Int

  /** The breakpoints that are set, by number */
  def breakpoints: Seq[(Int, String)]

  /** Removes a breakpoint, by number */
  def delete(number: Int): Unit

  /** Removes all the breakpoints */
  def clear(): Unit
}

object DebuggerAPI {

  /** Called before [[breakpoint]] stops, by the debugger, so that it can get ready */
  @volatile private var beforeBreakpoint: () => Unit = { () =>
    System.err.println(
      "breakpoint() called, but the debugger isn't enabled (start Ammonite with --debugger)"
    )
  }

  def setBeforeBreakpoint(f: () => Unit): Unit =
    beforeBreakpoint = f

  /** Stops the current thread in the debugger, like a breakpoint at the place of the call */
  def breakpoint(): Unit = {
    beforeBreakpoint()
    stopHere()
  }

  /**
   * The debugger sets a breakpoint in this method, and steps out of it when it's hit
   *
   * Returns its own line number, which is where the debugger sets its breakpoint.
   */
  def stopHere(): Int = new Exception().getStackTrace()(0).getLineNumber

  /** A [[DebuggerAPI]] of REPLs whose debugger isn't enabled */
  val disabled: DebuggerAPI = new DebuggerAPI {
    def enabled = false
    private def notEnabled() =
      throw new Exception("The debugger isn't enabled (start Ammonite with --debugger)")
    def break(location: String): Int = notEnabled()
    def breakpoints: Seq[(Int, String)] = Nil
    def delete(number: Int): Unit = notEnabled()
    def clear(): Unit = notEnabled()
  }
}
