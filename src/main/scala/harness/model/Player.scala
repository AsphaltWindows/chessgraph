package harness.model

import chess.model.{Chess, Types}

trait Player[T <: Types, C <: Chess[T]] {

  val types: T
  val chess: C

}
