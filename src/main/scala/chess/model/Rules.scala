package chess.model

trait Rules[T <: Types, P <: Position[T], M <: Move[T], G <: Game[T, P, M]] {

  val t: T

  def legalMoves(position: P): Seq[M]

  def playMove(game: G, move: M): G

}
