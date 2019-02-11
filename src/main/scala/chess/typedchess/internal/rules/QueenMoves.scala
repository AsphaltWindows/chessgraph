package chess.typedchess.internal.rules

import chess.typedchess.concrete.Moves.PieceMove
import chess.typedchess.concrete.TCMove
import chess.typedchess.internal.state.PieceTypes.Queen

object QueenMoves {


  import chess.typedchess.concrete.TCTypes._

  def queenMovesAndCaptures(square: Square, side: Side, pieceAt: Square => Option[Piece]): (Seq[(Square, TCMove)], Seq[(Square, TCMove)]) = {
    LinearMoves.linearMovesAndCaptures(
      queenVectors,
      allQueenMoveVectors,
      allQueenCaptureVectors,
      square,
      side,
      pieceAt)
  }

  val queenVectors = Lanes.laneVectors ++ Diagonals.diagonalVectors

  val allQueenMoveVectors: Map[Side, Map[Square, Map[Square, PieceMove]]] = LinearMoves
    .allLinearMoveVectors(
      Queen,
      Lanes.laneVectors ++ Diagonals.diagonalVectors
    )

  val allQueenCaptureVectors: Map[Side, Map[Square, Map[Square, PieceMove]]] = LinearMoves
    .allLinearCaptureVectors(
      Queen,
      queenVectors
    )

  val allMovesFlattened: Seq[TCMove] = LinearMoves.flattenMoveVectors(allQueenMoveVectors) ++
    LinearMoves.flattenMoveVectors(allQueenCaptureVectors)

}
