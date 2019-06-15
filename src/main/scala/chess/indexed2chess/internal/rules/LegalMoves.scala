package chess.indexed2chess.internal.rules

import chess.indexed2chess.concrete.{ICGame, ICMove, ICPosition}
import chess.indexed2chess.internal.state.PieceTypes._

object LegalMoves {

  import chess.indexed2chess.concrete.ICTypes._

  def legalNextMovesPositions(position: ICPosition): Seq[(ICMove, ICPosition)] = {
    val toMove = position.toMove
    val allPieces = position.allPieces(toMove)
      .groupBy(_._2.pieceType)

    def isFree(square: Square): Boolean = position.onSquare(square).isEmpty

    def isOpponentPiece(side: Side, square: Square): Boolean = position.onSquare(square).exists {
      _.side != side
    }

    def pieceAt(square: Square): Option[Piece] = position.onSquare(square)

    (allPieces
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
      } ++
      CastleMoves.castleMoves(
        position.toMove,
        Threats.isSquareAttacked(position),
        isFree,
        position.longCastleMap(position.toMove),
        position.shortCastleMap(position.toMove)
      )
      )
      .map { move =>
        (
          move,
          newPosition(position, move)
        )
      }
      .filter { case (_, nPos) =>
        !Threats.isKingInCheck(nPos, toMove)
      }
  }

  def newPosition(position: ICPosition, move: ICMove): ICPosition = {
    val copy = ICPosition.copy(position)
    copy.handleOps(
      MovePositionOps
        .movePositionsOpsMap(move)
    )
    copy
  }

}
