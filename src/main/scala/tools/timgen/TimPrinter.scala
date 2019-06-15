package tools.timgen

import chess.indexed2chess.Indexed2Chess
import chess.indexed2chess.concrete.Moves.{LongCastle, NonCastle, ShortCastle}
import chess.indexed2chess.internal.state.{Board, White}

object TimPrinter {

  val game = Indexed2Chess

  def positionString(p: game.Pos): String = {

    val possibleMoves = game.rules.legalNextMoves(p).map(_._1)

    val (nonCastle, castle) = possibleMoves.partition {
      case move: NonCastle => true
      case _ => false
    }

    val movesByFromIdx = nonCastle.collect {
      case n: NonCastle => n
    }
      .groupBy(_.from)
      .map { case (f, v) =>
        p.index.pieceIndexes(f) -> v
      }



    val TURN = if (p.toMove == White) "W" else "B"
    val INDICES = Board.allSquares.map { s =>
      p.index.pieceIndexes.get(s).map(_.toString).getOrElse("")
    }.mkString(",")
    val PIECES = Board.allSquares.map { s =>
      p.index.boardMap.get(s).map(_.pieceType.symbol).getOrElse("")
    }.mkString(",")
    val HASMOVED = (1 to 32).map { i =>
      p.index.hasMoved(i).toString
    }.mkString(",")
    val ENPASSANT = p.enPassant.map(_.file.symbol).getOrElse("")

    val MOVES = (if (p.toMove == White) (1 to 16) else (17 to 32))
      .map { idx =>
        val tos = movesByFromIdx.getOrElse(idx, Seq()).map(_.to).toSet
        if (tos.isEmpty) ""
        else {
          Board
            .allSquares
            .map { t =>
              if (tos.contains(t)) idx.toString else ""
            }
            .mkString(",")
        }
      }
      .mkString("|")

    val CASTLE = Seq(
      if (castle.exists {
        case l: LongCastle => true
        case _ => false
      }) 1 else 0,
      if (castle.exists {
        case s: ShortCastle => true
        case _ => false
      }) 1 else 0
    )
      .map(_.toString)
      .mkString("")

    Seq(
      TURN,
      INDICES,
      PIECES,
      HASMOVED,
      ENPASSANT,
      MOVES,
      CASTLE
    )
      .mkString("|")
  }

}
