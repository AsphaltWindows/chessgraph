package chess.typedchess.concrete

import chess.model.Rules

object TCRules extends Rules[TCTypes.type, TCPosition, TCMove, TCGame] {
  override val types: TCTypes.type = TCTypes

  override def legalNext(position: TCPosition): Seq[(TCMove, TCPosition)] = ???


  override def playMove(game: TCGame, move: TCMove): TCGame = ???
}
