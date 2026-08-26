package ammonite.compiler.internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.TermRef

/**
 * Reads the name a term reference points at.
 *
 * `NamedType.designator` went from returning a `Designator` to returning a `Showable` in
 * 3.8.2, so the call has to be made against the compiler it will run with.
 */
trait TermRefNames {

  /** The decoded name `ref` points at, if it points at a name or a symbol. */
  def designatorName(ref: TermRef)(using Context): Option[String]
}
