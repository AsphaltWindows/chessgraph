package chess.model

trait Game[T <: Types, P <: Position[T], M <: Move[T]] {

  val t: T

  def moveHistory: Seq[M]

  def positionHistory: Seq[P]

  def positionFrequency(position: P): Int

  def fiftyMoveCount: Int
}

