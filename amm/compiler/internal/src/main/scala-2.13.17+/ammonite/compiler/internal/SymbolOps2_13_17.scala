package ammonite.compiler.internal

import scala.tools.nsc.Global

/** The [[SymbolOps]] for the scalac of 2.13.17 and later. */
class SymbolOps2_13_17 extends SymbolOps {
  def isPackage(g: Global)(symbol: g.Symbol): Boolean = symbol.isPackage
}
