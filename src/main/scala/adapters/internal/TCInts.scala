package adapters.internal

import chess.typedchess.concrete.TCTypes
import chess.typedchess.concrete.TCTypes.Piece
import chess.typedchess.internal.state.PieceTypes.{Knight, Pawn}
import chess.typedchess.internal.state.{Black, White}

object TCInts {

  import chess.typedchess.internal.state.Pieces._

  def pieceToArray: Map[Option[Piece], Array[Byte]] = Map(
    None -> Array(0, 0, 0, 0, 0, 0, 0, 0).map(_.toByte),
    Some(WhitePawn) -> Array(1, 0, 1, 0, 0, 0, 0, 0).map(_.toByte),
    Some(BlackPawn) -> Array(0, 1, 1, 0, 0, 0, 0, 0).map(_.toByte),
    Some(WhiteKnight) -> Array(1, 0, 0, 1, 0, 0, 0, 0).map(_.toByte),
    Some(BlackKnight) -> Array(0, 1, 0, 1, 0, 0, 0, 0).map(_.toByte),
    Some(WhiteBishop) -> Array(1, 0, 0, 0, 1, 0, 0, 0).map(_.toByte),
    Some(BlackBishop) -> Array(0, 1, 0, 0, 1, 0, 0, 0).map(_.toByte),
    Some(WhiteRook) -> Array(1, 0, 0, 0, 0, 1, 0, 0).map(_.toByte),
    Some(BlackRook) -> Array(0, 1, 0, 0, 0, 1, 0, 0).map(_.toByte),
    Some(WhiteQueen) -> Array(1, 0, 0, 0, 0, 0, 1, 0).map(_.toByte),
    Some(BlackQueen) -> Array(0, 1, 0, 0, 0, 0, 1, 0).map(_.toByte),
    Some(WhiteKing) -> Array(1, 0, 0, 0, 0, 0, 0, 1).map(_.toByte),
    Some(BlackKing) -> Array(0, 1, 0, 0, 0, 0, 0, 1).map(_.toByte)
  )
}
