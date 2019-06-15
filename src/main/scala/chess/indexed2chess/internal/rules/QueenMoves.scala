package chess.indexed2chess.internal.rules

import chess.indexed2chess.concrete.Moves.PieceMove
import chess.indexed2chess.concrete.ICMove
import chess.indexed2chess.internal.state.PieceTypes.Queen

object QueenMoves {


  import chess.indexed2chess.concrete.ICTypes._

  def queenMovesAndCaptures(square: Square, side: Side, pieceAt: Square => Option[Piece]): (Seq[(Square, ICMove)], Seq[(Square, ICMove)]) = {
    LinearMoves.linearMovesAndCaptures(
      queenVectors,
      allQueenMoveVectors,
      allQueenCaptureVectors,
      square,
      side,
      pieceAt)
  }

  val queenVectors = (Lanes.laneVectors.toSeq ++ Diagonals.diagonalVectors.toSeq)
    .groupBy(_._1)
    .map { case (from, vecTups) =>
      from -> vecTups
        .flatMap(_._2)
    }
  val allQueenMoveVectors: Map[Side, Map[Square, Map[Square, PieceMove]]] = LinearMoves
    .allLinearMoveVectors(
      Queen,
      queenVectors
    )

  val allQueenCaptureVectors: Map[Side, Map[Square, Map[Square, PieceMove]]] = LinearMoves
    .allLinearCaptureVectors(
      Queen,
      queenVectors
    )

  val allMovesFlattened: Seq[ICMove] = LinearMoves.flattenMoveVectors(allQueenMoveVectors) ++
    LinearMoves.flattenMoveVectors(allQueenCaptureVectors)

}
