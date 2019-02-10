package chess.typedchess.internal.rules

import chess.typedchess.concrete.TCMove

object BishopMoves {

  import chess.typedchess.concrete.TCTypes._
  import chess.typedchess.internal.state.Pieces._

  def bishopMovesAndCaptures(square: Square, side: Side, pieceAt: Square => Option[Piece]): (Seq[TCMove], Seq[TCMove]) = {
    LinearMoves.linearMovesAndCaptures(
      Diagonals.diagonalVectors,
      allBishopMoveVectors,
      allBishopCaptureVectors,
      square,
      side,
      pieceAt)
  }

  val allBishopMoveVectors: Map[Side, Map[Square, Map[Square, TCMove]]] = LinearMoves
    .allLinearCaptureVectors(
      Bishop,
      Diagonals.diagonalVectors
    )

  val allBishopCaptureVectors: Map[Side, Map[Square, Map[Square, TCMove]]] = LinearMoves
    .allLinearCaptureVectors(
      Bishop,
      Diagonals.diagonalVectors
    )


}
