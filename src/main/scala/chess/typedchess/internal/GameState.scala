package chess.typedchess.internal

sealed trait GameState

case object WhiteWin extends GameState
case object BlackWin extends GameState
case object Draw extends GameState
case object InProgress extends GameState