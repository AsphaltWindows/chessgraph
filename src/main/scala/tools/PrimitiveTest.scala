package tools

import chess.typedchess.TypedChess

object PrimitiveTest extends App {

  override def main(args: Array[String]): Unit = {

    val chess = TypedChess

    val game1 = chess.newGame

    println(game1.currentPosition.id)

    val nextMoves = chess
      .rules
      .legalNextMoves(game1.currentPosition)

    println(nextMoves.size)

    nextMoves.foreach {case (mv, pos) =>
      println(mv)
      println(pos.id)
    }

  }
}
