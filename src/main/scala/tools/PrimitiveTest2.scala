package tools

import chess.typedchess.TypedChess
import chess.typedchess.internal.notation.LongAlgebraic

object PrimitiveTest2 extends App {

  override def main(args: Array[String]): Unit = {

    val chess = TypedChess

    val game1 = chess.newGame
    val moves: Seq[String] = Seq(
      "e2-e4", "e7-e5",
      "Bf1-c4", "h7-h6",
      "Qd1-f3", "a7-a6",
      "Qf3xf7"
    )

    moves
      .foreach { alg =>
        val pos = game1.currentPosition
        val mov = LongAlgebraic.longAlgToMoveMap(pos.toMove)(alg)
        chess.rules.advanceGame(game1, mov, chess.rules.newPosition(pos, mov))
      }

    val nextMoves = chess
      .rules
      .legalNextMoves(game1.currentPosition)

    println(nextMoves.size + " legal moves:")

    nextMoves.foreach {case (mv, pos) =>
      println(LongAlgebraic.moveToLongAlgMap(mv))
    }

    println(nextMoves.size + " resulting positions:")

    nextMoves.foreach {case (mv, pos) =>
      println(pos.id)
    }
  }
}
