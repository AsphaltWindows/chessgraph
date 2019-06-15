package chess.indexed2chess.internal.rules

import chess.indexed2chess.concrete.Moves.PieceMove
import chess.indexed2chess.concrete.ICMove
import chess.indexed2chess.internal.state.PieceTypes.Rook

object RookMoves {

  import chess.indexed2chess.concrete.ICTypes._

  def rookMovesAndCaptures(square: Square, side: Side, pieceAt: Square => Option[Piece]): (Seq[(Square, ICMove)], Seq[(Square, ICMove)]) = {
    LinearMoves.linearMovesAndCaptures(
      Lanes.laneVectors,
      allRookMoveVectors,
      allRookCaptureVectors,
      square,
      side,
      pieceAt)
  }

  val allRookMoveVectors: Map[Side, Map[Square, Map[Square, PieceMove]]] = LinearMoves
    .allLinearMoveVectors(
      Rook,
      Lanes.laneVectors
    )

  val allRookCaptureVectors: Map[Side, Map[Square, Map[Square, PieceMove]]] = LinearMoves
    .allLinearCaptureVectors(
      Rook,
      Lanes.laneVectors
    )

  val allMovesFlattened: Seq[ICMove] = LinearMoves.flattenMoveVectors(allRookMoveVectors) ++
    LinearMoves.flattenMoveVectors(allRookCaptureVectors)

}
