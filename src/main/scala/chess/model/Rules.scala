package chess.model

trait Rules[T <: Types, P <: Position[T], M <: Move[T], G <: Game[T, P, M]] {

  val types: T

  def legalNext(position: P): Seq[(M, P)]

  def playMove(game: G, move: M): G

}
