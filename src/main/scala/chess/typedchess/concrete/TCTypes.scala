package chess.typedchess.concrete

import chess.model.Types
import chess.typedchess.internal.state.Board.TCSquare
import chess.typedchess.internal.state.Pieces._
import chess.typedchess.internal.state._

object TCTypes extends Types {

  override type PieceType = TCPiece

  override type File = TCFile

  override type Rank = TCRank

  override type Side = SideColor

  override type State = GameState

  override type Square = Board.Square

  private case class FullPiece(side: Side, pieceType: PieceType)

  override type Piece = FullPiece

  override def sq(file: TCFile, rank: TCRank): Square = TCSquare.apply(file, rank)

  override def pce(side: SideColor, pieceType: TCPiece): Piece = FullPiece.apply(side, pieceType)


  object FullPiece {

    def apply(side: Side, pieceType: PieceType): FullPiece = {
      pieceMap(side)(pieceType)
    }
  }

  val WhitePawn = FullPiece(White, Pawn)
  val WhiteKnight = FullPiece(White, Knight)
  val WhiteBishop = FullPiece(White, Bishop)
  val WhiteRook = FullPiece(White, Rook)
  val WhiteQueen = FullPiece(White, Queen)
  val WhiteKing = FullPiece(White, King)

  val BlackPawn = FullPiece(Black, Pawn)
  val BlackKnight = FullPiece(Black, Knight)
  val BlackBishop = FullPiece(Black, Bishop)
  val BlackRook = FullPiece(Black, Rook)
  val BlackQueen = FullPiece(Black, Queen)
  val BlackKing = FullPiece(Black, King)

  private val pieceMap: Map[Side, Map[PieceType, FullPiece]] = Map(
    White -> Map(
      Pawn -> WhitePawn,
      Knight -> WhiteKnight,
      Bishop -> WhiteBishop,
      Rook -> WhiteRook,
      Queen -> WhiteQueen
    ),
    Black -> Map(
      Pawn -> BlackPawn,
      Knight -> BlackKnight,
      Bishop -> BlackBishop,
      Rook -> BlackRook,
      Queen -> BlackQueen
    )
  )
}
