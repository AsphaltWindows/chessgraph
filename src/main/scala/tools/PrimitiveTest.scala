package tools

import chess.typedchess.TypedChess

object PrimitiveTest extends App {

  override def main(args: Array[String]): Unit = {

    val chess = TypedChess

    val game1 = chess.newGame

    println(game1.currentPosition.id)
  }
}
