package tools.timgen

import chess.indexed2chess.Indexed2Chess
import chess.indexed2chess.internal.notation.LongAlgebraic

import scala.util.Random

object TimGame {

  val game = Indexed2Chess

  def makeMove(g: game.Gam): game.Mov = {
    val legal = game.rules.legalNextMoves(g)
    val toPlay = legal(Random.nextInt(legal.size))
    println(LongAlgebraic.moveToLongAlgMap(toPlay._1))
    game.rules.advanceGame(g, toPlay._1, toPlay._2)
    toPlay._1
  }

}
