package harness.model

import chess.model.{Chess, Types}

trait Match[T <: Types, C <: Chess[T], P1 <: Player[T, C], P2 <: Player[T, C]] {

  type Score

  val types: T
  val chess: C
  val player1: P1
  val player2: P2

  def playMatch: Score
}
