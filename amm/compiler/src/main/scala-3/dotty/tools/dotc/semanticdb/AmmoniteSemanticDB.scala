package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.core.Phases.Phase

object AmmoniteSemanticDB {
  def extractSemanticInfo: Phase = new ExtractSemanticDB.ExtractSemanticInfo
}
