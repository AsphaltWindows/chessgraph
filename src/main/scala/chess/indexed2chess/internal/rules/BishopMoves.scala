package chess.indexed2chess.internal.rules

import chess.indexed2chess.concrete.Moves.{NonCastle, PieceMove}
import chess.indexed2chess.concrete.ICMove

object BishopMoves {

  import chess.indexed2chess.concrete.ICTypes._
  import chess.indexed2chess.internal.state.PieceTypes._

  def bishopMovesAndCaptures(square: Square, side: Side, pieceAt: Square => Option[Piece]): (Seq[(Square, ICMove)], Seq[(Square, ICMove)]) = {
    LinearMoves.linearMovesAndCaptures(
      Diagonals.diagonalVectors,
      allBishopMoveVectors,
      allBishopCaptureVectors,
      square,
      side,
      pieceAt)
  }

  val allBishopMoveVectors: Map[Side, Map[Square, Map[Square, PieceMove]]] = LinearMoves
    .allLinearMoveVectors(
      Bishop,
      Diagonals.diagonalVectors
    )

  val allBishopCaptureVectors: Map[Side, Map[Square, Map[Square, PieceMove]]] = LinearMoves
    .allLinearCaptureVectors(
      Bishop,
      Diagonals.diagonalVectors
    )

  val allMovesFlattened: Seq[ICMove] = LinearMoves.flattenMoveVectors(allBishopMoveVectors) ++
    LinearMoves.flattenMoveVectors(allBishopCaptureVectors)

}
