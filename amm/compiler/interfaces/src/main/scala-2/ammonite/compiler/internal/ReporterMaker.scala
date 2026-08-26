package ammonite.compiler.internal

import scala.reflect.internal.util.Position
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.Reporter

/**
 * Creates the reporter Ammonite drives the Scala compiler with.
 *
 * scalac replaced `AbstractReporter` with `FilteringReporter` in 2.12.13 / 2.13.1, and gave
 * `FilteringReporter.doReport` an extra list of code actions in 2.13.12.
 */
trait ReporterMaker {
  def makeReporter(
      errorLogger: (Position, String) => Unit,
      warningLogger: (Position, String) => Unit,
      infoLogger: (Position, String) => Unit,
      settings: Settings
  ): Reporter
}
