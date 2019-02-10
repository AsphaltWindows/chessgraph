package chess.model

trait Rules[T <: Types, P <: Position[T], M <: Move[T], G <: Game[T, P, M]] {

  val types: T

  def legalNextMoves(position: P): Seq[(M, P)]

  def advanceGame(game: G, move: M, position: P): G

}
