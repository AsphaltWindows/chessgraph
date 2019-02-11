package chess.typedchess.internal.state

object Pieces {

  import PieceTypes._

  sealed case class TCPiece(side: SideColor, pieceType: TCPieceType)

  val WhitePawn = TCPiece(White, Pawn)
  val WhiteKnight = TCPiece(White, Knight)
  val WhiteBishop = TCPiece(White, Bishop)
  val WhiteRook = TCPiece(White, Rook)
  val WhiteQueen = TCPiece(White, Queen)
  val WhiteKing = TCPiece(White, King)

  val BlackPawn = TCPiece(Black, Pawn)
  val BlackKnight = TCPiece(Black, Knight)
  val BlackBishop = TCPiece(Black, Bishop)
  val BlackRook = TCPiece(Black, Rook)
  val BlackQueen = TCPiece(Black, Queen)
  val BlackKing = TCPiece(Black, King)

  val pieceMap: Map[SideColor, Map[TCPieceType, TCPiece]] = Map(
    White -> Map(
      Pawn -> WhitePawn,
      Knight -> WhiteKnight,
      Bishop -> WhiteBishop,
      Rook -> WhiteRook,
      Queen -> WhiteQueen,
      King -> WhiteKing
    ),
    Black -> Map(
      Pawn -> BlackPawn,
      Knight -> BlackKnight,
      Bishop -> BlackBishop,
      Rook -> BlackRook,
      Queen -> BlackQueen,
      King -> BlackKing
    )
  )
}
