package ammonite.debugger

/** Where the debugger logs things */
trait Logger {
  def error(msg: => String, t: Throwable = null): Unit
  def warn(msg: => String, t: Throwable = null): Unit
  def info(msg: => String, t: Throwable = null): Unit
  def debug(msg: => String, t: Throwable = null): Unit
}

object Logger {

  /** Discards everything */
  val nop: Logger = new Logger {
    def error(msg: => String, t: Throwable): Unit = ()
    def warn(msg: => String, t: Throwable): Unit = ()
    def info(msg: => String, t: Throwable): Unit = ()
    def debug(msg: => String, t: Throwable): Unit = ()
  }

  /** Prints messages of level `minLevel` and above (0: debug, 1: info, 2: warn, 3: error) */
  def stderr(minLevel: Int): Logger = new Logger {
    private def log(level: Int, name: String, msg: => String, t: Throwable): Unit =
      if (level >= minLevel) {
        System.err.println(s"[$name] $msg")
        if (t != null) t.printStackTrace(System.err)
      }
    def error(msg: => String, t: Throwable): Unit = log(3, "error", msg, t)
    def warn(msg: => String, t: Throwable): Unit = log(2, "warn", msg, t)
    def info(msg: => String, t: Throwable): Unit = log(1, "info", msg, t)
    def debug(msg: => String, t: Throwable): Unit = log(0, "debug", msg, t)
  }
}
