package chess.typedchess.internal.state

sealed trait SideColor

case object White extends SideColor
case object Black extends SideColor

object SideColor {
  def other(side: SideColor): SideColor = side match {
    case White => Black
    case Black => White
  }
}
