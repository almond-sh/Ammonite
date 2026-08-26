package ammonite.compiler.internal

import scala.tools.nsc.Global

/**
 * The parts of scalac's symbol API that moved between patch releases.
 *
 * A call to one of these compiles to a reference to whichever class or trait declares it in
 * the scalac of the day, so it has to be made against the compiler it will run with -
 * `isPackage`, for one, left `HasFlags` in 2.13.17.
 */
trait SymbolOps {

  /** Whether `symbol` is a package. */
  def isPackage(g: Global)(symbol: g.Symbol): Boolean
}
