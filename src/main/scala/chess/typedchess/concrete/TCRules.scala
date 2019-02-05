package chess.typedchess.concrete

import chess.model.Rules

object TCRules extends Rules[TCTypes.type, TCPosition, TCMove, TCGame] {
  override val t: TCTypes.type = TCTypes

  override def legalMoves(position: TCPosition): Seq[TCMove] = ???


  override def playMove(game: TCGame, move: TCMove): TCGame = ???
}
