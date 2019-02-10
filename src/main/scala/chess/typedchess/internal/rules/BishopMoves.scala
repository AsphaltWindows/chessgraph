package chess.typedchess.internal.rules

import chess.typedchess.concrete.TCMove

object BishopMoves {

  import chess.typedchess.concrete.TCTypes._
  import chess.typedchess.internal.state.Pieces._

  def bishopMovesAndCaptures(square: Square, side: Side, pieceAt: Square => Option[Piece]): (Seq[TCMove], Seq[TCMove]) = {
    val vectors = Diagonals.diagonalVectors(square)

    LinearMoves.linearMovesAndCaptures(Bishop, vectors, square, side, pieceAt)
  }

}
