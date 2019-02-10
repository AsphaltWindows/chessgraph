package chess.typedchess.internal.rules

import chess.typedchess.concrete.TCMove
import chess.typedchess.internal.state.Pieces.Queen

object QueenMoves {


  import chess.typedchess.concrete.TCTypes._

  def queenMovesAndCaptures(square: Square, side: Side, pieceAt: Square => Option[Piece]): (Seq[TCMove], Seq[TCMove]) = {
    val vectors = Lanes.laneVectors(square) ++ Diagonals.diagonalVectors(square)

    LinearMoves.linearMovesAndCaptures(Queen, vectors, square, side, pieceAt)
  }

}
