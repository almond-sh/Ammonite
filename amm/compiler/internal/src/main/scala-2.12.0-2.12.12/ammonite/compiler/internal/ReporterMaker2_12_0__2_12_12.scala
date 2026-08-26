package ammonite.compiler.internal

import ammonite.util.Classpath

import scala.reflect.internal.util.Position
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.AbstractReporter

/** The [[ReporterMaker]] for the scalac reporters of 2.12.0 to 2.12.12. */
class ReporterMaker2_12_0__2_12_12 extends ReporterMaker {
  def makeReporter(
      errorLogger: (Position, String) => Unit,
      warningLogger: (Position, String) => Unit,
      infoLogger: (Position, String) => Unit,
      outerSettings: Settings
  ): AbstractReporter =
    new AbstractReporter {
      def displayPrompt(): Unit = ???

      def display(pos: Position, msg: String, severity: Severity): Unit =
        severity match {
          case ERROR =>
            Classpath.traceClasspathProblem(s"ERROR: $msg")
            errorLogger(pos, msg)
          case WARNING =>
            warningLogger(pos, msg)
          case INFO =>
            infoLogger(pos, msg)
        }

      val settings = outerSettings
    }
}
