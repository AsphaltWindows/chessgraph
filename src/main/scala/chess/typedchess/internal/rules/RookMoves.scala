package chess.typedchess.internal.rules

import chess.typedchess.concrete.Moves.PieceMove
import chess.typedchess.concrete.TCMove
import chess.typedchess.internal.state.Pieces.Rook

object RookMoves {

  import chess.typedchess.concrete.TCTypes._

  def rookMovesAndCaptures(square: Square, side: Side, pieceAt: Square => Option[Piece]): (Seq[TCMove], Seq[TCMove]) = {
    LinearMoves.linearMovesAndCaptures(
      Lanes.laneVectors,
      allRookMoveVectors,
      allRookCaptureVectors,
      square,
      side,
      pieceAt)
  }

  val allRookMoveVectors: Map[Side, Map[Square, Map[Square, PieceMove]]] = LinearMoves
    .allLinearCaptureVectors(
      Rook,
      Lanes.laneVectors
    )

  val allRookCaptureVectors: Map[Side, Map[Square, Map[Square, PieceMove]]] = LinearMoves
    .allLinearCaptureVectors(
      Rook,
      Lanes.laneVectors
    )

  val allMovesFlattened: Seq[TCMove] = LinearMoves.flattenMoveVectors(allRookMoveVectors) ++
    LinearMoves.flattenMoveVectors(allRookCaptureVectors)

}
