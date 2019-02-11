package chess.typedchess.internal.rules

import chess.typedchess.concrete.TCPosition
import chess.typedchess.internal.state.PieceTypes._

object Threats {

  import chess.typedchess.concrete.TCTypes._

  def isSquareAttacked(position: TCPosition)(square: Square, attackedBy: Side): Boolean = {
    position
      .allPieces(attackedBy)
      .exists { case (at, piece) =>
        piece.pieceType match {
          case Pawn => PawnMoves.allPawnCaptures(attackedBy)(at).exists(_.to == square)
          case Knight => KnightMoves.allKnightCaptures(attackedBy)(at).exists(_._1 == square)
          case Bishop => {
            Diagonals.squareToDiagonal(at).exists(_.contains(square)) && {
              val (_, captures) = BishopMoves.bishopMovesAndCaptures(at, attackedBy, position.onSquare)
              captures.exists(_._1 == square)
            }
          }
          case Rook => {
            Lanes.squareToLane(at).exists(_.contains(square)) && {
              val (_, captures) = RookMoves.rookMovesAndCaptures(at, attackedBy, position.onSquare)
              captures.exists(_._1 == square)
            }
          }
          case Queen => {
            (Lanes.squareToLane(at).exists{_.contains(square)} ||
              Diagonals.squareToDiagonal(at).exists{_.contains(square)}) && {
              val (_, captures) = QueenMoves.queenMovesAndCaptures(at, attackedBy, position.onSquare)
              captures.exists(_._1 == square)
            }
          }
          case King => KingMoves.allKingCaptures(attackedBy)(at).exists(_._1 == square)
        }
      }
  }

}
