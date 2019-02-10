package chess.typedchess.internal.rules

import chess.typedchess.concrete.TCMove

object KnightMoves {

  import chess.typedchess.concrete.TCTypes._
  import chess.typedchess.internal.state.Board._

  private val allKnightSquares: Map[Square, Seq[Square]] = allSquares
    .map { case s@TCSquare(f, r) =>
      val bigLateral = Seq(
        f + 2,
        f - 2
      )
        .flatten
      val smallLateral = Seq(
        f + 1,
        f - 1
      )
        .flatten

      val bigVertical = Seq(
        r + 2,
        r - 2
      )
        .flatten

      val smallVertical = Seq(
        r + 1,
        r - 1
      )
        .flatten

      (bigLateral
        .flatMap { nf =>
          smallVertical
            .map { nr =>
              sq(nf, nr)
            }
        }.toSeq ++
        smallLateral
          .flatMap { nf =>
            bigVertical
              .map { nr =>
                sq(nf, nr)
              }
          }.toSeq)
    }
    .toMap

  val validRank =
}
