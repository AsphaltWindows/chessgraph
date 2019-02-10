package chess.typedchess.internal.rules

import chess.typedchess.concrete.TCMove
import chess.typedchess.internal.state.Pieces.Rook

object RookMoves {

  import chess.typedchess.concrete.TCTypes._

  def rookMovesAndCaptures(square: Square, side: Side, pieceAt: Square => Option[Piece]): (Seq[TCMove], Seq[TCMove]) = {
    val vectors = Lanes.laneVectors(square)

    LinearMoves.linearMovesAndCaptures(Rook, vectors, square, side, pieceAt)
  }

}
