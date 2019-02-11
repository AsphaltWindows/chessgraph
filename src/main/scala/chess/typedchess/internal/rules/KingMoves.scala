package chess.typedchess.internal.rules

import chess.typedchess.concrete.TCMove
import chess.typedchess.internal.state.PieceTypes.{King, Knight}
import chess.typedchess.internal.state.{Black, White}

object KingMoves {

  import chess.typedchess.concrete.Moves._
  import chess.typedchess.concrete.TCTypes._
  import chess.typedchess.internal.state.Board._

  def kingMoves(square: Square, side: Side, squareFree: Square => Boolean): Seq[TCMove] = {
    allKingMoves(side)(square)
      .filter{ case (k, v) => squareFree(k)}
      .values
      .toSeq
  }

  def kingCaptures(square: Square, side: Side, opponentPieceAt: (Side, Square) => Boolean): Seq[TCMove] = {
    allKingMoves(side)(square)
      .filter{ case (k, v) => opponentPieceAt(side, k)}
      .values
      .toSeq
  }

  private val allKingSquares: Map[Square, Seq[Square]] = allSquares
    .map { case s@TCSquare(f, r) =>
      val lateral = Seq(
        f - 1,
        Option(f),
        f + 1
      )
        .flatten
      val vertical = Seq(
        r - 1,
        Option(r),
        r + 1
      )
        .flatten

      s-> lateral
        .flatMap { nf =>
          vertical
            .map { nr =>
              sq(nf, nr)
            }
        }
    }
    .toMap

  val allKingMoves: Map[Side, Map[Square, Map[Square, PieceMove]]] = Map(
    White -> allKingSquares
      .map { case (from, tos) =>
        from -> tos.map { to =>
          to -> PieceMoveWhite(King, from, to)
        }
          .toMap
      },
    Black -> allKingSquares
      .map { case (from, tos) =>
        from -> tos.map { to =>
          to -> PieceMoveBlack(King, from, to)
        }
          .toMap
      }
  )

  val allKingCaptures: Map[Side, Map[Square, Map[Square, PieceMove]]] = Map(
    White -> allKingSquares
      .map { case (from, tos) =>
        from -> tos.map { to =>
          to -> PieceCaptureWhite(King, from, to)
        }
          .toMap
      },
    Black -> allKingSquares
      .map { case (from, tos) =>
        from -> tos.map { to =>
          to -> PieceCaptureBlack(King, from, to)
        }
          .toMap
      }
  )

  val allMovesFlattened: Seq[TCMove] = allKingMoves.toSeq.flatMap { case (_, moveFromMap) =>
    moveFromMap.flatMap { case (_, moveToMap) =>
      moveToMap.values
    }
  } ++ allKingCaptures.toSeq.flatMap { case (_, moveFromMap) =>
    moveFromMap.flatMap { case (_, moveToMap) =>
      moveToMap.values
    }
  }
}
