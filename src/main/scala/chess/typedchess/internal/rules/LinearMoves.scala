package chess.typedchess.internal.rules

import chess.typedchess.concrete.Moves.{PieceCaptureBlack, PieceCaptureWhite, PieceMoveBlack, PieceMoveWhite}
import chess.typedchess.concrete.TCMove
import chess.typedchess.internal.state.Pieces.Linear
import chess.typedchess.internal.state.{Black, White}

object LinearMoves {

  import chess.typedchess.concrete.TCTypes._

  def linearMovesAndCaptures(pieceType: Linear,
                             moveVectors: Seq[Seq[Square]],
                             square: Square,
                             side: Side,
                             pieceAt: Square => Option[Piece]): (Seq[TCMove], Seq[TCMove]) = {
    moveVectors
      .map { vector =>
        vector
          .foldLeft[(Seq[TCMove], Option[TCMove], Boolean)](Seq(), None, true) {
          case ((moves, capture, false), _) => (moves, capture, false)
          case ((moves, _, true), nextSquare) =>
            val pieceOpt = pieceAt(nextSquare)
            if (pieceOpt.isEmpty) {
              ((side match {
                case White => PieceMoveWhite(pieceType, square, nextSquare)
                case Black => PieceMoveBlack(pieceType, square, nextSquare)
              }) +: moves, None, true)
            }
            else if (pieceOpt.exists(_ != side)) {
              (
                moves,
                Option(side match {
                  case White => PieceCaptureWhite(pieceType, square, nextSquare)
                  case Black => PieceCaptureBlack(pieceType, square, nextSquare)
                }),
                false
              )
            }
            else {
              (moves, None, false)
            }
        }
      }
      .foldLeft[(Seq[TCMove], Seq[TCMove])](Seq(), Seq()) {
      case ((movesAcc, capturesAcc), (moves, capturesOpt, _)) => (moves ++ movesAcc, capturesOpt.toSeq ++ capturesAcc)
    }
  }
}
