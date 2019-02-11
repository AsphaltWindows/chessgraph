package chess.typedchess.internal.state

import chess.typedchess.concrete.TCTypes

import scala.collection.mutable
import scala.collection.mutable.{Map => MutMap, Set => MutSet}

abstract class PositionIndex() {

  import TCTypes._

  val boardMap: MutMap[Square, Piece]
  val pieceMap: Map[Piece, MutSet[Square]]
  val sideSquareMap: Map[Side, MutSet[Square]]

  def remove(square: Square): Unit = {

    val pieceOpt = boardMap.get(square)
    pieceOpt
      .foreach { case piece =>
        boardMap -= square
        pieceMap(piece) -= square
        sideSquareMap(piece.side) -= square
      }
  }

  def place(piece: Piece, square: Square): Unit = {
    boardMap.update(square, piece)
    pieceMap(piece) += square
    sideSquareMap(piece.side) += square
  }

  def reposition(from: Square, to: Square): Unit = {

    val pieceOpt = boardMap.get(from)
    remove(from)
    pieceOpt.foreach { piece =>
      place(piece, to)
    }
  }
}

object PositionIndex {

  import Board._
  import TCTypes._
  import Pieces._

  def copy(index: PositionIndex): PositionIndex = new PositionIndex {
    override val boardMap: mutable.Map[Square, Piece] = MutMap() ++= index.boardMap
    override val pieceMap: Map[Piece, mutable.Set[Square]] = {
      index
        .pieceMap
        .map { case (k, v) =>
          k -> (MutSet() ++= v)
        }
    }
    override val sideSquareMap: Map[SideColor, mutable.Set[Square]] = {
      index
        .sideSquareMap
        .map { case (k, v) =>
          k -> (MutSet() ++= v)
        }
    }
  }

  private val initialBoard: Map[Square, Piece] = Map(
    A1 -> WhiteRook,
    B1 -> WhiteKnight,
    C1 -> WhiteBishop,
    D1 -> WhiteQueen,
    E1 -> WhiteKing,
    F1 -> WhiteBishop,
    G1 -> WhiteKnight,
    H1 -> WhiteRook,
    A2 -> WhitePawn,
    B2 -> WhitePawn,
    C2 -> WhitePawn,
    D2 -> WhitePawn,
    E2 -> WhitePawn,
    F2 -> WhitePawn,
    G2 -> WhitePawn,
    H2 -> WhitePawn,
    A7 -> BlackPawn,
    B7 -> BlackPawn,
    C7 -> BlackPawn,
    D7 -> BlackPawn,
    E7 -> BlackPawn,
    F7 -> BlackPawn,
    G7 -> BlackPawn,
    H7 -> BlackPawn,
    A8 -> BlackRook,
    B8 -> BlackKnight,
    C8 -> BlackBishop,
    D8 -> BlackQueen,
    E8 -> BlackKing,
    F8 -> BlackBishop,
    G8 -> BlackKnight,
    H8 -> BlackRook
  )


  private val initialPiece: Map[Piece, Set[Square]] = Map(
    WhiteRook -> Set(A1, H1),
    WhiteKnight -> Set(B1, G1),
    WhiteBishop -> Set(C1, F1),
    WhiteQueen -> Set(D1),
    WhiteKing -> Set(E1),
    WhitePawn -> Set(A2, B2, C2, D2, E2, F2, G2, H2),
    BlackRook -> Set(A8, H8),
    BlackKnight -> Set(B8, G8),
    BlackBishop -> Set(C8, F8),
    BlackQueen -> Set(D8),
    BlackKing -> Set(B8),
    BlackPawn -> Set(A7, B7, C7, D7, E7, F7, G7, H7)
  )

  private val initialSideMap: Map[Side, Set[Square]] = Map(
    White -> Set(A1, B1, C1, D1, E1, F1, G1, H1, A2, B2, C2, D2, E2, F2, G2, H2),
    Black -> Set(A8, B8, C8, D8, E8, F8, G8, H8, A7, B7, C7, D7, E7, F7, G7, H7)
  )

  def init(): PositionIndex = new PositionIndex {
    override val boardMap: MutMap[Square, Piece] = MutMap() ++= initialBoard
    override val pieceMap: Map[Piece, MutSet[Square]] = Map() ++ initialPiece.map { case (k, v) => k -> (MutSet() ++= v) }
    override val sideSquareMap: Map[Side, MutSet[Square]] = Map() ++ initialSideMap.map { case (k, v) => k -> (MutSet() ++= v) }
  }

}