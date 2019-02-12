package chess.typedchess.concrete

import chess.model.Rules
import chess.typedchess.concrete.TCGame.SetState
import chess.typedchess.internal.rules._
import chess.typedchess.internal.state.PieceTypes._
import chess.typedchess.internal.state._

object TCRules extends Rules[TCTypes.type, TCPosition, TCMove, TCGame] {
  override val types: TCTypes.type = TCTypes

  override def legalNextMoves(game: TCGame): Seq[(TCMove, TCPosition)] = LegalMoves.legalNextMovesPositions(game.currentPosition)

  override def legalNextMoves(position: TCPosition): Seq[(TCMove, TCPosition)] = LegalMoves.legalNextMovesPositions(position)

  def newPosition(position: TCPosition, move: TCMove): TCPosition = {
    val copy = TCPosition.copy(position)
    copy.handleOps(
      MovePositionOps
        .movePositionsOpsMap(move)
    )
    copy
  }

  override def advanceGame(game: TCGame, move: TCMove, position: TCPosition): TCGame = {
    game.handleOps(
      MovePositionOps.gameOps(move, position)
    )

    game.handleOps(
      Seq(
        SetState(
          GameStateConditions.calculateGameState(game)
        )
      )
    )

    game
  }

}
