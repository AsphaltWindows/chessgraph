package chess.typedchess.concrete

import chess.model.Game

abstract class TCGame extends Game[TCRules.type, TCPosition, TCMove] {
  override val t: TCRules.type = _

  override def moveHistory: Seq[TCMove] = ???

  override def positionHistory: Seq[TCPosition] = ???

  override def positionFrequency(position: TCPosition): Int = ???
}

object TCGame {


}

