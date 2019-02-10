package chess.model

trait Game[T <: Types, P <: Position[T], M <: Move[T]] {

  val types: T

  def position: P

  def moveHistory: Seq[M]

  def positionHistory: Seq[P]

  def positionFrequency(position: P): Int

  def gameState: types.State

  def fiftyMoveCount: Int
}

