package chess.typedchess.concrete

import chess.model.Position
import chess.typedchess.concrete.TCTypes.FullPiece
import chess.typedchess.internal.state.Board.TCSquare
import chess.typedchess.internal.state.Pieces.King
import chess.typedchess.internal.state._

import scala.collection.mutable.{Map => MutMap}

class TCPosition extends Position[TCTypes.type] {

  import TCPosition._

  override val types: TCTypes.type = TCTypes

  import TCTypes._

  override def toMove: Side = sideToMove

  override def onSquare(square: Square): Option[Piece] = index.boardMap.get(square)

  override def allSquares: Map[Square, Piece] = index.boardMap.toMap

  override def allPieces(side: Side): Seq[(Square, Piece)] = index
    .sideSquareMap(side)
    .map { square =>
      (square, index.boardMap(square))
    }
    .toSeq

  override def findKing(side: Side): Square = index.pieceMap(pce(side, King)).head

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
      .map { case (sq, pc) =>
        sq.file.symbol + sq.rank.symbol + (pc.side match { case White => "W" case Black => "B"}) + pc.pieceType.symbol
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
      case DisableLongCastle(side) => longCastleMap.update(side, false)
      case DisableShortCastle(side) => shortCastleMap.update(side, false)
      case SetEnPassant(square) =>
        squareEnPassant = Some(square)
      case ResetEnPassant =>
        squareEnPassant = None
      case FlipMove =>
        sideToMove = SideColor.other(sideToMove)
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

  case class DisableLongCastle(side: Side) extends PositionOp

  case class DisableShortCastle(side: Side) extends PositionOp

  case class SetEnPassant(square: Square) extends PositionOp

  case object ResetEnPassant extends PositionOp

  case object FlipMove extends PositionOp

}