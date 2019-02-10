package chess.model

trait Move[T <: Types] {
  val types: T
}
