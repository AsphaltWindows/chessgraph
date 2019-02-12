package chess.typedchess.concrete

import chess.model.Rules
import chess.typedchess.concrete.TCGame.SetState
import chess.typedchess.internal.rules._
import chess.typedchess.internal.state.PieceTypes._
import chess.typedchess.internal.state._

object TCRules extends Rules[TCTypes.type, TCPosition, TCMove, TCGame] {
  override val types: TCTypes.type = TCTypes

  override def legalNextMoves(position: TCPosition): Seq[(TCMove, TCPosition)] = {
    val toMove = position.toMove
    val allPieces = position.allPieces(toMove)
      .groupBy(_._2.pieceType)

    def isFree(square: types.Square): Boolean = position.onSquare(square).isEmpty

    def isOpponentPiece(side: types.Side, square: types.Square): Boolean = position.onSquare(square).exists {
      _.side != side
    }

    def pieceAt(square: types.Square): Option[types.Piece] = position.onSquare(square)

    allPieces
      .toSeq
      .flatMap {
        case (Pawn, pieces) =>
          pieces.flatMap { case (square, _) =>
            PawnMoves.pawnAdvances(square, toMove, isFree) ++
              PawnMoves.pawnCaptures(square, toMove, isOpponentPiece) ++
              position
                .enPassant
                .toSeq
                .flatMap { enpassant =>
                  PawnMoves.pawnEnPassant(square, toMove, enpassant)
                }
          }
        case (Knight, pieces) =>
          pieces.flatMap { case (square, _) =>
            KnightMoves.knightMoves(square, toMove, isFree) ++
              KnightMoves.knightCaptures(square, toMove, isOpponentPiece)
          }
        case (Bishop, pieces) =>
          pieces.flatMap { case (square, _) =>
            val (moves, caps) =
              BishopMoves.bishopMovesAndCaptures(
                square,
                toMove,
                pieceAt
              )
            (moves ++ caps).map(_._2)
          }
        case (Rook, pieces) =>
          pieces.flatMap { case (square, _) =>
            val (moves, caps) =
              RookMoves.rookMovesAndCaptures(
                square,
                toMove,
                pieceAt
              )
            (moves ++ caps).map(_._2)
          }
        case (Queen, pieces) =>
          pieces.flatMap { case (square, _) =>
            val (moves, caps) =
              QueenMoves.queenMovesAndCaptures(
                square,
                toMove,
                pieceAt
              )
            (moves ++ caps).map(_._2)
          }
        case (King, pieces) =>
          pieces.flatMap { case (square, _) =>
            KingMoves.kingMoves(square, toMove, isFree) ++
              KingMoves.kingCaptures(square, toMove, isOpponentPiece)
          }
      }
      .map { move =>
        (
          move,
          newPosition(position, move)
        )
      }
      .filter { case (_, nPos) =>
        !isKingInCheck(nPos, toMove)
      }
  }

  def newPosition(position: TCPosition, move: TCMove): TCPosition = {
    val copy = TCPosition.copy(position)
    copy.handleOps(
      MovePositionOps
        .movePositionsOpsMap(move)
    )
    copy
  }

  override def advanceGame(game: TCGame, move: TCMove, position: TCPosition): TCGame = {
    game.handleOps(
      MovePositionOps.gameOps(move, position)
    )

    game.handleOps(
      Seq(
        SetState(
          calculateState(game)
        )
      )
    )

    game
  }

  private def calculateState(game: TCGame): types.State = {
    if (game.fiftyMoveCount == 100) {
      Draw
    }
    else if (game.positionFrequency(game.position) >= 3) {
      Draw
    }
    else if (legalNextMoves(game.position).isEmpty) {
      if (isKingInCheck(game.position, game.position.toMove)) {
        game.position.toMove match {
          case White => BlackWin
          case Black => WhiteWin
        }
      }
      else {
        Draw
      }
    }
    else {
      InProgress
    }
  }

  private def isKingInCheck(position: TCPosition, sideOfKing: types.Side): Boolean = {
    Threats.isSquareAttacked(position)(position.findKing(sideOfKing), SideColor.other(sideOfKing))
  }
}
