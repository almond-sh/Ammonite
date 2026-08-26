package ammonite.compiler.internal

import scala.tools.nsc.Global

/** The [[SymbolOps]] for the scalac of 2.13.0 to 2.13.16. */
class SymbolOps2_13_0__2_13_16 extends SymbolOps {
  def isPackage(g: Global)(symbol: g.Symbol): Boolean = symbol.isPackage
}
