package chess.model

trait Position[T <: Types] {

  val types: T

  def toMove: types.Side

  def longCastle(side: types.Side): Boolean
  def shortCastle(side: types.Side): Boolean

  def onSquare(square: types.Square): Option[types.Piece]
  def allSquares: Map[types.Square, types.Piece]

  def enPassant: Option[types.Square]

  def id: String
}
