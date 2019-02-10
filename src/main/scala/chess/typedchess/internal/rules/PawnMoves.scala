package chess.typedchess.internal.rules

import chess.typedchess.concrete.Moves._
import chess.typedchess.concrete.TCMove
import chess.typedchess.internal.state.{Black, Board, TCFile, White}

object PawnMoves {

  import Board._
  import chess.typedchess.concrete.TCTypes._
  import chess.typedchess.internal.state.Pieces._

  def pawnAdvances(square: Square, side: Side, squareFree: Square => Boolean): Seq[TCMove] = {
    side match {
      case White => whitePawnMoves(square).filter { move =>
        squareFree(move.to)
      }
      case Black => blackPawnMoves(square).filter { move =>
        squareFree(move.to)
      }
    }
  }

  def pawnCaptures(square: Square, side: Side, opposingPiece: (Side, Square) => Boolean): Seq[TCMove] = {
    side match {
      case White => whitePawnCaptures(square).filter { move =>
        opposingPiece(side, square)
      }
      case Black => blackPawnCaptures(square).filter { move =>
        opposingPiece(side, square)
      }
    }
  }

  def pawnEnPassant(square: Square, side: Side, enPassant: Square): Seq[TCMove] = {
    side match {
      case White => whitePawnEnPassant(square).filter { move =>
        move.enpassant == enPassant
      }
      case Black => blackPawnEnPassant(square).filter { move =>
        move.enpassant == enPassant
      }
    }
  }

  private val whitePawnMoves: Map[Square, Seq[PawnMove]] = allSquares
    .map { case s@TCSquare(f, r) =>
      s -> (
        if (r == `1` || r == `8`) {
          Seq()
        }
        else if (r == `2`) {
          Seq(
            PawnAdvanceWhite(s, sq(f, `3`)),
            PawnDoubleAdvanceWhite(s, sq(f, `4`))
          )
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
              PawnAdvanceBlack(s, TCSquare(f, nr))
            }
        })
    }.toMap

  private val blackPawnMoves: Map[Square, Seq[PawnMove]] = allSquares
    .map { case s@TCSquare(f, r) =>
      s -> (if (r == `1` || r == `8`) {
        Seq()
      }
      else if (r == `7`) {
        Seq(
          PawnAdvanceBlack(s, sq(f, `6`)),
          PawnDoubleAdvanceBlack(s, sq(f, `5`))
        )
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
            PawnAdvanceBlack(s, TCSquare(f, nr))
          }
      })
    }.toMap

  private val validCaptureFiles: Map[TCFile, Seq[TCFile]] = allFiles
    .map { f =>
      f -> ((f - 1).toSeq ++ (f + 1).toSeq)
    }
    .toMap

  private val whitePawnCaptures: Map[Square, Seq[PawnMove]] = allSquares
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
    .toMap

  private val blackPawnCaptures: Map[Square, Seq[PawnMove]] = allSquares
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
                  PawnCapturePromoteWhite(s, sq(file, nr), Knight),
                  PawnCapturePromoteWhite(s, sq(file, nr), Bishop),
                  PawnCapturePromoteWhite(s, sq(file, nr), Rook),
                  PawnCapturePromoteWhite(s, sq(file, nr), Queen)
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
                  PawnCaptureWhite(s, sq(file, nr))
                }
            }
        })
    }
    .toMap

  private val whitePawnEnPassant: Map[Square, Seq[EnPassant]] = allSquares
    .map {
      case s@TCSquare(f, `5`) => s -> validCaptureFiles(f)
        .map { file =>
          PawnEnPassantWhite(s, sq(file, `6`), sq(file, `5`))
        }
      case s => s -> Seq()
    }
    .toMap

  private val blackPawnEnPassant: Map[Square, Seq[EnPassant]] = allSquares
    .map {
      case s@TCSquare(f, `4`) => s -> validCaptureFiles(f)
        .map { file =>
          PawnEnPassantWhite(s, sq(file, `3`), sq(file, `4`))
        }
      case s => s -> Seq()
    }
    .toMap

}
