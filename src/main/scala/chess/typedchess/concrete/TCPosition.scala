package chess.typedchess.concrete

import chess.model.Position
import chess.typedchess.concrete.TCTypes.FullPiece
import chess.typedchess.internal.state.Board.TCSquare
import chess.typedchess.internal.state.{Black, Board, PositionIndex, White}

import scala.collection.mutable.{Map => MutMap}

class TCPosition extends Position[TCTypes.type] {

  import TCPosition._

  override val types: TCTypes.type = TCTypes

  type Side = types.Side
  type Square = types.Square
  type Piece = types.Piece

  override def toMove: Side = sideToMove

  override def onSquare(square: Square): Option[Piece] = index.boardMap.get(square)

  override def allSquares: Map[Square, Piece] = index.boardMap.toMap

  override def longCastle(side: Side): Boolean = longCastleMap(side)

  override def shortCastle(side: Side): Boolean = shortCastleMap(side)

  override def enPassant: Option[Square] = squareEnPassant

  override def id: String = {

    val turn = sideToMove match {
      case White => "W"
      case Black => "B"
    }
    val wsc = if (shortCastleMap(White)) "1" else "0"
    val wlc = if (longCastleMap(White)) "1" else "0"
    val bsc = if (shortCastleMap(Black)) "1" else "0"
    val blc = if (longCastleMap(Black)) "1" else "0"
    val pieces = Board
      .allSquares
      .flatMap { square =>
        index
          .boardMap
          .get(square)
          .map((square, _))
      }
      .map { case (TCSquare(f, r), FullPiece(s, p)) =>
        f.symbol + r.symbol + (s match { case White => "W" case Black => "B"}) + p.symbol
      }
      .mkString("")

    val enp = enPassant.map{case TCSquare(f, r) => "e" + f.symbol + r.symbol}

    turn + wsc + wlc + bsc + blc + pieces + enp
  }

  def handleOps(ops: Seq[PositionOp]): Unit = {
    ops.foreach {
      case Remove(square) => index.remove(square)
      case Place(piece, on) => index.place(piece, on)
      case Reposition(from, to) => index.reposition(from, to)
    }
  }

  var sideToMove: Side = White
  var squareEnPassant: Option[Square] = None
  val longCastleMap: MutMap[Side, Boolean] = MutMap(White -> true, Black -> true)
  val shortCastleMap: MutMap[Side, Boolean] = MutMap(White -> true, Black -> true)
  protected val index: PositionIndex = PositionIndex.init()
}


object TCPosition {

  import TCTypes._

  def copy(pos: TCPosition): TCPosition = {
    new TCPosition {
      sideToMove = pos.sideToMove
      squareEnPassant = pos.squareEnPassant
      override protected val index: PositionIndex = PositionIndex.copy(pos.index)
      override val longCastleMap: MutMap[Side, Boolean] = MutMap() ++ longCastleMap
      override val shortCastleMap: MutMap[Side, Boolean] = MutMap() ++ shortCastleMap
    }
  }

  trait PositionOp

  case class Remove(square: Square) extends PositionOp

  case class Place(piece: Piece, to: Square) extends PositionOp

  case class Reposition(from: Square, to: Square) extends PositionOp

}