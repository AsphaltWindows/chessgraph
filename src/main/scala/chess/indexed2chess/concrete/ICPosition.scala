package chess.indexed2chess.concrete

import chess.model.Position
import chess.indexed2chess.internal.state.Board.ICSquare
import chess.indexed2chess.internal.state.PieceTypes.King
import chess.indexed2chess.internal.state.Pieces.ICPiece
import chess.indexed2chess.internal.state._

import scala.collection.mutable.{Map => MutMap}

class ICPosition extends Position[ICTypes.type] {

  import ICPosition._

  override val types: ICTypes.type = ICTypes

  import ICTypes._

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
      .map { case (ICSquare(f, r), ICPiece(sd, pc)) =>
        f.symbol + r.symbol + (sd match { case White => "W" case Black => "B"}) + pc.symbol
      }
      .mkString("")

    val enp = enPassant.map{ case ICSquare(f, r) => "e" + f.symbol + r.symbol}.getOrElse("")

    turn + wsc + wlc + bsc + blc + pieces + enp
  }

  def handleOps(ops: Seq[PositionOp]): Unit = {
    ops.foreach {
      case Remove(square) => index.remove(square)
      case Replace(piece, at) => index.replace(piece, at)
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
  val index: PositionIndex = PositionIndex.init()

}


object ICPosition {

  import ICTypes._

  def copy(pos: ICPosition): ICPosition = {
    new ICPosition {
      sideToMove = pos.sideToMove
      squareEnPassant = pos.squareEnPassant
      override val index: PositionIndex = PositionIndex.copy(pos.index)
      override val longCastleMap: MutMap[Side, Boolean] = MutMap() ++= pos.longCastleMap
      override val shortCastleMap: MutMap[Side, Boolean] = MutMap() ++= pos.shortCastleMap
    }
  }

  trait PositionOp

  case class Remove(square: Square) extends PositionOp

  case class Replace(piece: Piece, at: Square) extends PositionOp

  case class Reposition(from: Square, to: Square) extends PositionOp

  case class DisableLongCastle(side: Side) extends PositionOp

  case class DisableShortCastle(side: Side) extends PositionOp

  case class SetEnPassant(square: Square) extends PositionOp

  case object ResetEnPassant extends PositionOp

  case object FlipMove extends PositionOp

}