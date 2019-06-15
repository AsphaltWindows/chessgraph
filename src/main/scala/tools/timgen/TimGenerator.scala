package tools.timgen

import java.io.{File, PrintWriter}

import chess.indexed2chess.Indexed2Chess._
import chess.indexed2chess.Indexed2Chess
import chess.indexed2chess.concrete.ICGame
import chess.indexed2chess.internal.state.InProgress
import chess.indexed2chess.internal.notation.LongAlgebraic
import tools.draw.DrawBoard

import scala.util.Try

/**
  * Parameters:
  * Absolute path to folder to store games
  * File Name Prefix
  * Number of games to generate
  * Optional: maximum number of moves per game
  */
object TimGenerator extends App {

  val chess = Indexed2Chess

  override def main(args: Array[String]): Unit = {
    val pathArg = args(0)
    val prefixArg = args(1)
    val gameNum = args(2).toInt
    val maxMoveNum = Try(args(3)).toOption.map(_.toInt)

    (1 to gameNum)
      .foreach { num =>

        val pwTim = new PrintWriter(new File(s"$pathArg/${prefixArg}_$num.game"))
        val pwHuman = new PrintWriter(new File(s"$pathArg/human_$num.game"))

        val game = new ICGame

        pwTim.println(TimPrinter.positionString(game.currentPosition))
        pwHuman.println(DrawBoard.draw(game.currentPosition))

        while (game.gameState == InProgress && !maxMoveNum.exists { l => l*2 < game.moveHistory.size }) {
          val movePlayed = TimGame.makeMove(game)
          pwTim.println(TimPrinter.positionString(game.currentPosition))
          pwTim.flush()
          pwHuman.println(s"${(game.moveHistory.size - 1)/2 + 1}${if(game.moveHistory.size % 2 == 1) "" else ".."}${LongAlgebraic.moveToLongAlgMap(movePlayed)}")
          pwHuman.println(DrawBoard.draw(game.currentPosition))
          pwHuman.flush()
        }

        pwTim.close()
        pwHuman.println(game.gameState)
        pwHuman.flush()
        pwHuman.close()
        println(s"gamenum: $num ${(game.moveHistory.size + 1)/2} moves ${game.gameState} ${game.moveHistory.map(m => LongAlgebraic.moveToLongAlgMap(m))}")

      }
  }


}
