package chess.typedchess.internal.rules

import chess.typedchess.concrete.Moves._
import chess.typedchess.concrete.TCMove
import chess.typedchess.internal.state.{Black, Board, TCFile, White}

object PawnMoves {

  import Board._
  import chess.typedchess.concrete.TCTypes._
  import chess.typedchess.internal.state.PieceTypes._

  def pawnDoubleAdvances(square: Square, side: Side, squareFree: Square => Boolean): Seq[TCMove] = {

    allPawnDoubleAdvances(side)(square).filter { move =>
      squareFree(move.to) &&
        (if (side == White)
          move.from.rank + 1
        else
          move.from.rank - 1)
          .map { rank =>
            sq(move.from.file, rank)
          }
          .exists { s =>
            squareFree(s)
          }
    }
  }

  def pawnAdvances(square: Square, side: Side, squareFree: Square => Boolean): Seq[TCMove] = {

    allPawnAdvances(side)(square).filter { move =>
      squareFree(move.to)
    }
  }

  def pawnCaptures(square: Square, side: Side, opponentPieceAt: (Side, Square) => Boolean): Seq[TCMove] = {
    allPawnCaptures(side)(square).filter { move =>
      opponentPieceAt(side, move.to)
    }
  }

  def pawnEnPassant(square: Square, side: Side, enPassant: Square): Seq[TCMove] = {

    allPawnEnPassant(side)(square).filter { move =>
      move.enpassant == enPassant
    }
  }

  val allPawnDoubleAdvances: Map[Side, Map[Square, Seq[PawnMove]]] = Map(
    White -> allSquares
      .map { case s@TCSquare(f, r) =>
        s -> (
          if (r == `2`) {
            Seq(
              PawnDoubleAdvanceWhite(s, sq(f, `4`))
            )
          }
          else {
            Seq()
          })
      }.toMap,
    Black -> allSquares
      .map { case s@TCSquare(f, r) =>
        s -> (
          if (r == `7`) {
            Seq(
              PawnDoubleAdvanceBlack(s, sq(f, `5`))
            )
          }
          else {
            Seq()
          })
      }.toMap
  )

  val allPawnAdvances: Map[Side, Map[Square, Seq[PawnMove]]] = Map(
    White -> allSquares
      .map { case s@TCSquare(f, r) =>
        s -> (
          if (r == `1` || r == `8`) {
            Seq()
          }
          else if (r == `7`) {
            Seq(
              PawnAdvancePromoteWhite(s, sq(f, `8`), Knight),
              PawnAdvancePromoteWhite(s, sq(f, `8`), Bishop),
              PawnAdvancePromoteWhite(s, sq(f, `8`), Queen),
              PawnAdvancePromoteWhite(s, sq(f, `8`), Rook)
            )
          }
          else {
            (r + 1)
              .toSeq
              .map { nr =>
                PawnAdvanceWhite(s, sq(f, nr))
              }
          })
      }.toMap,
    Black -> allSquares
      .map { case s@TCSquare(f, r) =>
        s -> (if (r == `1` || r == `8`) {
          Seq()
        }
        else if (r == `2`) {
          Seq(
            PawnAdvancePromoteBlack(s, sq(f, `1`), Knight),
            PawnAdvancePromoteBlack(s, sq(f, `1`), Bishop),
            PawnAdvancePromoteBlack(s, sq(f, `1`), Queen),
            PawnAdvancePromoteBlack(s, sq(f, `1`), Rook)
          )
        }
        else {
          (r - 1)
            .toSeq
            .map { nr =>
              PawnAdvanceBlack(s, sq(f, nr))
            }
        })
      }.toMap
  )

  private val validCaptureFiles: Map[TCFile, Seq[TCFile]] = allFiles
    .map { f =>
      f -> ((f - 1).toSeq ++ (f + 1).toSeq)
    }
    .toMap

  val allPawnCaptures: Map[Side, Map[Square, Seq[PawnMove]]] = Map(
    White -> allSquares
      .map { case s@TCSquare(f, r) =>
        val validFiles = validCaptureFiles(f)
        s -> (
          if (r == `1` || r == `8`) {
            Seq()
          }
          else if (r == `7`) {
            validFiles.flatMap { file =>
              (r + 1)
                .toSeq
                .flatMap { nr =>
                  Seq(
                    PawnCapturePromoteWhite(s, sq(file, nr), Knight),
                    PawnCapturePromoteWhite(s, sq(file, nr), Bishop),
                    PawnCapturePromoteWhite(s, sq(file, nr), Rook),
                    PawnCapturePromoteWhite(s, sq(file, nr), Queen)
                  )
                }
            }
          }
          else {
            validFiles.flatMap { file =>
              (r + 1)
                .toSeq
                .map { nr =>
                  PawnCaptureWhite(s, sq(file, nr))
                }
            }
          })
      }
      .toMap,
    Black -> allSquares
      .map { case s@TCSquare(f, r) =>
        val validFiles = validCaptureFiles(f)
        s -> (
          if (r == `1` || r == `8`) {
            Seq()
          }
          else if (r == `2`) {
            validFiles.flatMap { file =>
              (r - 1)
                .toSeq
                .flatMap { nr =>
                  Seq(
                    PawnCapturePromoteBlack(s, sq(file, nr), Knight),
                    PawnCapturePromoteBlack(s, sq(file, nr), Bishop),
                    PawnCapturePromoteBlack(s, sq(file, nr), Rook),
                    PawnCapturePromoteBlack(s, sq(file, nr), Queen)
                  )
                }
            }
          }
          else {
            validFiles
              .flatMap { file =>
                (r - 1)
                  .toSeq
                  .map { nr =>
                    PawnCaptureBlack(s, sq(file, nr))
                  }
              }
          })
      }
      .toMap
  )

  val allPawnEnPassant: Map[Side, Map[Square, Seq[EnPassant]]] = Map(
    White -> allSquares
      .map {
        case s@TCSquare(f, `5`) => s -> validCaptureFiles(f)
          .map { file =>
            PawnEnPassantWhite(s, sq(file, `6`), sq(file, `5`))
          }
        case s => s -> Seq()
      }
      .toMap,
    Black -> allSquares
      .map {
        case s@TCSquare(f, `4`) => s -> validCaptureFiles(f)
          .map { file =>
            PawnEnPassantWhite(s, sq(file, `3`), sq(file, `4`))
          }
        case s => s -> Seq()
      }
      .toMap
  )

  val allMovesFlattened: Seq[TCMove] = allPawnAdvances.toSeq.flatMap { case (_, moveFromMap) =>
    moveFromMap.flatMap(_._2)
  } ++ allPawnDoubleAdvances.toSeq.flatMap { case (_, moveFromMap) =>
    moveFromMap.flatMap(_._2)
  } ++ allPawnCaptures.toSeq.flatMap { case (_, moveFromMap) =>
    moveFromMap.flatMap(_._2)
  } ++ allPawnEnPassant.toSeq.flatMap { case (_, moveFromMap) =>
    moveFromMap.flatMap(_._2)
  }
}
