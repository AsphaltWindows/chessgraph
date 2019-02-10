package chess.typedchess.concrete

import chess.model.Notation

object TCNotation extends Notation[TCTypes.type, TCPosition, TCMove] {
  override def print(position: TCPosition): String = "lol"

  override def print(move: TCMove): String = "lol2"

  override def print(moves: Seq[TCMove]): String = moves
    .grouped(2)
    .zipWithIndex
    .map { case (moves, idx) =>
      moves match {
        case wMove :: bMoveSeq => s"$idx.${print(wMove)}${bMoveSeq.headOption.map{ mv => " " + print(mv) + " "}.getOrElse("")}"
        case Nil => ""
      }
    }.mkString("")
}
