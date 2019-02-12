package tools

import chess.typedchess.TypedChess
import chess.typedchess.internal.notation.LongAlgebraic

object PrimitiveTest1 extends App {

  override def main(args: Array[String]): Unit = {

    val chess = TypedChess

    val game1 = chess.newGame

    println(game1.currentPosition.id)

    val nextMoves = chess
      .rules
      .legalNextMoves(game1)

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
