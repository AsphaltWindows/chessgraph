package chess.model

trait Game[T <: Types, P <: Position[T], M <: Move[T]] {

  val types: T

  def moveHistory: Seq[M]

  def positionHistory: Seq[P]

  def positionFrequency(position: P): Int

  def advanceGame(move: M, position: P): Unit

  def fiftyMoveCount: Int
}

