package chess.typedchess.internal.notation

import chess.typedchess.concrete.Moves._
import chess.typedchess.concrete.TCMove
import chess.typedchess.internal.rules._
import chess.typedchess.internal.state.Board

object LongAlgebraic {

  import chess.typedchess.concrete.TCTypes._

  val moveToLongAlgMap: Map[TCMove, String] = (CastleMoves.allMovesFlattened ++
    KingMoves.allMovesFlattened ++
    KnightMoves.allMovesFlattened ++
    PawnMoves.allMovesFlattened ++
    BishopMoves.allMovesFlattened ++
    RookMoves.allMovesFlattened ++
    QueenMoves.allMovesFlattened)
    .map { mov =>
      mov -> moveLongAlg(mov)
    }
    .toMap

  val longAlgToMoveMap: Map[Side, Map[String, Set[TCMove]]] = moveToLongAlgMap
    .toSeq
    .groupBy(_._1.side)
    .map { case (side, tups) =>
      side -> tups
        .groupBy(_._2)
        .map { case (notation, moves) =>
            notation -> moves.map(_._1).toSet
        }
    }

  def moveLongAlg(move: TCMove): String = move match {
    case c: Castle => castleLongAlg(c)
    case nc: NonCastle => nc.piece.symbol + Board.squareStrings(nc.from) + connector(nc) + Board.squareStrings(nc.to)
  }


  def castleLongAlg(castle: Castle): String = castle match {
    case s: ShortCastle => "O-O"
    case l: LongCastle => "O-O-O"
  }

  def connector(nc: NonCastle): String = nc match {
    case _: Capture => "x"
    case _ => "-"
  }

}
