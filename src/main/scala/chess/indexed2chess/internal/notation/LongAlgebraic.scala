package chess.indexed2chess.internal.notation

import chess.indexed2chess.concrete.Moves._
import chess.indexed2chess.concrete.ICMove
import chess.indexed2chess.internal.rules._
import chess.indexed2chess.internal.state.Board

object LongAlgebraic {

  import chess.indexed2chess.concrete.ICTypes._

  val moveToLongAlgMap: Map[ICMove, String] = (CastleMoves.allMovesFlattened ++
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

  val longAlgToMoveMap: Map[Side, Map[String, Set[ICMove]]] = moveToLongAlgMap
    .toSeq
    .groupBy(_._1.side)
    .map { case (side, tups) =>
      side -> tups
        .groupBy(_._2)
        .map { case (notation, moves) =>
          notation -> moves.map(_._1).toSet
        }
    }

  def moveLongAlg(move: ICMove): String = move match {
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
