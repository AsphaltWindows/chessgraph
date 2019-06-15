package chess.indexed2chess.internal.state

object Pieces {

  import PieceTypes._

  sealed case class ICPiece(side: SideColor, pieceType: ICPieceType)

  val WhitePawn = ICPiece(White, Pawn)
  val WhiteKnight = ICPiece(White, Knight)
  val WhiteBishop = ICPiece(White, Bishop)
  val WhiteRook = ICPiece(White, Rook)
  val WhiteQueen = ICPiece(White, Queen)
  val WhiteKing = ICPiece(White, King)

  val BlackPawn = ICPiece(Black, Pawn)
  val BlackKnight = ICPiece(Black, Knight)
  val BlackBishop = ICPiece(Black, Bishop)
  val BlackRook = ICPiece(Black, Rook)
  val BlackQueen = ICPiece(Black, Queen)
  val BlackKing = ICPiece(Black, King)

  val pieceMap: Map[SideColor, Map[ICPieceType, ICPiece]] = Map(
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
