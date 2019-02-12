package tools

import chess.typedchess.TypedChess
import chess.typedchess.internal.notation.LongAlgebraic
import chess.typedchess.internal.state._

import scala.io.StdIn

object InteractiveTest extends App {

  import chess.typedchess.concrete.TCTypes._

  override def main(args: Array[String]): Unit = {

    val chess = TypedChess

    val game1 = chess.newGame

    val toMovePrompt: Map[Side, String] = Map(
      White -> "white to move: ",
      Black -> "black to move: "
    )

    while (game1.gameState == InProgress) {
      val moveStr = StdIn.readLine(toMovePrompt(game1.currentPosition.toMove))
      val move = LongAlgebraic.longAlgToMoveMap(game1.currentPosition.toMove)(moveStr)
      chess.rules.advanceGame(game1, move, chess.rules.newPosition(game1.currentPosition, move))
    }

    game1
      .moveHistory
      .foreach { move=>
        println(LongAlgebraic.moveToLongAlgMap(move))
      }
    println(game1.gameState)
  }

}
