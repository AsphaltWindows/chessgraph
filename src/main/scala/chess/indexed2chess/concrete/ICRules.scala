package chess.indexed2chess.concrete

import chess.model.Rules
import chess.indexed2chess.concrete.ICGame.SetState
import chess.indexed2chess.internal.rules._
import chess.indexed2chess.internal.state.PieceTypes._
import chess.indexed2chess.internal.state._

object ICRules extends Rules[ICTypes.type, ICPosition, ICMove, ICGame] {
  override val types: ICTypes.type = ICTypes

  override def legalNextMoves(game: ICGame): Seq[(ICMove, ICPosition)] = LegalMoves.legalNextMovesPositions(game.currentPosition)

  override def legalNextMoves(position: ICPosition): Seq[(ICMove, ICPosition)] = LegalMoves.legalNextMovesPositions(position)

  def newPosition(position: ICPosition, move: ICMove): ICPosition = {
    val copy = ICPosition.copy(position)
    copy.handleOps(
      MovePositionOps
        .movePositionsOpsMap(move)
    )
    copy
  }

  override def advanceGame(game: ICGame, move: ICMove, position: ICPosition): ICGame = {
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
