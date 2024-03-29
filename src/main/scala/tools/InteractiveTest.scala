package tools

import chess.typedchess.TypedChess
import chess.typedchess.internal.notation.LongAlgebraic
import chess.typedchess.internal.state._
import tools.draw.DrawBoard
import scala.io.StdIn

object InteractiveTest extends App {

  import chess.typedchess.concrete.TCTypes._

  override def main(args: Array[String]): Unit = {

    val chess = TypedChess

    val game1 = chess.newGame

    val guiEnabled = true

    val toMovePrompt: Map[Side, String] = Map(
      White -> "white to move: ",
      Black -> "black to move: "
    )

    while (game1.gameState == InProgress) {
      val moveStr = StdIn.readLine(toMovePrompt(game1.currentPosition.toMove))
      val moveSet = LongAlgebraic.longAlgToMoveMap(game1.currentPosition.toMove).get(moveStr).getOrElse(Set())
      val moveOpt = chess
        .rules
        .legalNextMoves(game1)
        .map(_._1).find { m =>
        moveSet.contains(m)
      }
      moveOpt.foreach { move =>
        if (chess.rules.legalNextMoves(game1).map(_._1).contains(move)) {
          chess.rules.advanceGame(game1, move, chess.rules.newPosition(game1.currentPosition, move))
          if (guiEnabled) {
            print(DrawBoard.draw(game1.position))
          }
        }
      }
    }

    game1
      .moveHistory
      .foreach { move =>
        println(LongAlgebraic.moveToLongAlgMap(move))
      }
    println(game1.gameState)
  }

}
