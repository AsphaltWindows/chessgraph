package chess.model

trait Notation[T <: Types, P <: Position[T], M <: Move[T]] {

  def print(position: P): String

  def print(move: M): String

  def print(moves: Seq[M]): String

}
