package chess.indexed2chess.concrete

import chess.model.Notation

object ICNotation extends Notation[ICTypes.type, ICPosition, ICMove] {
  override def print(position: ICPosition): String = "lol"

  override def print(move: ICMove): String = "lol2"

  override def print(moves: Seq[ICMove]): String = moves
    .grouped(2)
    .zipWithIndex
    .map { case (moves, idx) =>
      moves match {
        case wMove :: bMoveSeq => s"$idx.${print(wMove)}${bMoveSeq.headOption.map{ mv => " " + print(mv) + " "}.getOrElse("")}"
        case Nil => ""
      }
    }.mkString("")
}
