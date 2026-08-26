package ammonite.compiler.internal

import scala.tools.nsc.Global

/** The [[SymbolOps]] for the scalac of 2.12. */
class SymbolOps2_12 extends SymbolOps {
  def isPackage(g: Global)(symbol: g.Symbol): Boolean = symbol.isPackage
}
