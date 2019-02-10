package chess.typedchess.internal.rules

import chess.typedchess.concrete.Moves.{PieceCaptureBlack, PieceCaptureWhite, PieceMoveBlack, PieceMoveWhite}
import chess.typedchess.concrete.TCMove
import chess.typedchess.internal.state.Pieces.Linear
import chess.typedchess.internal.state.{Black, White}

object LinearMoves {

  import chess.typedchess.concrete.TCTypes._

  def linearMovesAndCaptures(squareVectors: Map[Square, Seq[Seq[Square]]],
                             allMoves: Map[Side, Map[Square, Map[Square, TCMove]]],
                             allCaptures: Map[Side, Map[Square, Map[Square, TCMove]]],
                             square: Square,
                             side: Side,
                             pieceAt: Square => Option[Piece]): (Seq[TCMove], Seq[TCMove]) = {
    squareVectors(square)
      .map { vector =>
        val moveMap = allMoves(side)(square)
        val captureMap = allCaptures(side)(square)
        vector
          .foldLeft[(Seq[TCMove], Option[TCMove], Boolean)](Seq(), None, true) {
          case ((moves, capture, false), _) => (moves, capture, false)
          case ((moves, _, true), nextSquare) =>
            val pieceOpt = pieceAt(nextSquare)
            if (pieceOpt.isEmpty) {
              (moveMap(nextSquare) +: moves, None, true)
            }
            else if (pieceOpt.exists(_ != side)) {
              (moves, captureMap.get(nextSquare), false)
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

  def allLinearMoveVectors(pieceType: Linear,
                           moveVectorMap: Map[Square, Seq[Seq[Square]]]): Map[Side, Map[Square, Map[Square, TCMove]]] = {
    Map(
      White -> moveVectorMap
        .map { case (from, vectors) =>
          from -> vectors
            .flatMap { vector =>
              vector.map { to =>
                (to, PieceMoveWhite(pieceType, from, to))
              }
            }
            .toMap
        },
      Black -> moveVectorMap
        .map { case (from, vectors) =>
          from -> vectors
            .flatMap { vector =>
              vector.map { to =>
                (to, PieceMoveBlack(pieceType, from, to))
              }
            }
            .toMap
        }
    )
  }

  def allLinearCaptureVectors(pieceType: Linear,
                              captureVectorMap: Map[Square, Seq[Seq[Square]]]): Map[Side, Map[Square, Map[Square, TCMove]]] = {
    Map(
      White -> captureVectorMap
        .map { case (from, vectors) =>
          from -> vectors
            .flatMap { vector =>
              vector.map { to =>
                (to, PieceCaptureWhite(pieceType, from, to))
              }
            }
            .toMap
        },
      Black -> captureVectorMap
        .map { case (from, vectors) =>
          from -> vectors
            .flatMap { vector =>
              vector.map { to =>
                (to, PieceCaptureBlack(pieceType, from, to))
              }
            }
            .toMap
        }
    )
  }
}
