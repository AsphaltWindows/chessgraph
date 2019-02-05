package chess.model

trait Position[T <: Types] {

  val t: T

  type Side = t.Side
  type Rank = t.Rank
  type File = t.File
  type Piece = t.Piece
  type Square = t.Square

  def moveOf: Side

  def longCastle(side: Side): Boolean
  def shortCastle(side: Side): Boolean

  def onSquare(square: Square): Option[Piece]
  def allSquares: Map[Square, Piece]

}
