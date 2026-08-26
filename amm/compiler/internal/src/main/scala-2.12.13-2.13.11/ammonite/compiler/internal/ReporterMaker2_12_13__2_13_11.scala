package ammonite.compiler.internal

import ammonite.util.Classpath

import scala.reflect.internal.util.Position
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.FilteringReporter

/** The [[ReporterMaker]] for the scalac reporters of 2.12.13 to 2.13.11. */
class ReporterMaker2_12_13__2_13_11 extends ReporterMaker {
  def makeReporter(
      errorLogger: (Position, String) => Unit,
      warningLogger: (Position, String) => Unit,
      infoLogger: (Position, String) => Unit,
      outerSettings: Settings
  ): FilteringReporter =
    new FilteringReporter {

      def doReport(pos: Position, msg: String, severity: Severity): Unit =
        display(pos, msg, severity)

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

      def settings = outerSettings
    }
}
