package chess.typedchess.concrete

import chess.model.Position
import chess.typedchess.internal._

abstract class TCPosition extends Position[TCTypes.type]{

  override val t: TCTypes.type = TCTypes

  override def moveOf: Side = index.sideToMove

  override def longCastle(side: Side): Boolean = side match {
    case White => index.whiteLongCastle
    case Black => index.blackLongCastle
  }

  override def shortCastle(side: Side): Boolean = side match {
    case White => index.whiteShortCastle
    case Black => index.blackShortCastle
  }

  override def onSquare(square: Square): Option[Piece] = index.boardMap.get(square)

  override def allSquares: Map[Square, Piece] = index.boardMap.toMap

  protected[TCRules] val index: PositionIndex

}


object TCPosition {

  val initial: TCPosition = new TCPosition {
    protected val index: PositionIndex = PositionIndex.init()
  }

}