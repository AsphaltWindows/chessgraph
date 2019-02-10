package chess.typedchess.internal.rules

import chess.typedchess.concrete.TCMove
import chess.typedchess.internal.state.Pieces.Queen

object QueenMoves {


  import chess.typedchess.concrete.TCTypes._

  def queenMovesAndCaptures(square: Square, side: Side, pieceAt: Square => Option[Piece]): (Seq[TCMove], Seq[TCMove]) = {
    LinearMoves.linearMovesAndCaptures(
      queenVectors,
      allQueenMoveVectors,
      allQueenCaptureVectors,
      square,
      side,
      pieceAt)
  }

  val allQueenMoveVectors: Map[Side, Map[Square, Map[Square, TCMove]]] = LinearMoves
    .allLinearCaptureVectors(
      Queen,
      Lanes.laneVectors ++ Diagonals.diagonalVectors
    )

  val allQueenCaptureVectors: Map[Side, Map[Square, Map[Square, TCMove]]] = LinearMoves
    .allLinearCaptureVectors(
      Queen,
      queenVectors
    )

  val queenVectors = Lanes.laneVectors ++ Diagonals.diagonalVectors

}
