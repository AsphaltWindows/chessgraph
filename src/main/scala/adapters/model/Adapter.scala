package adapters.model

import chess.model.{Chess, Types}

trait Adapter[T <: Types, C <: Chess[T], Input] {

  val types: T
  val chess: C

  def positionAsInput(position: chess.Pos): Input

}
