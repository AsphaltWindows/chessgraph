package chess.typedchess.internal.state

object Pieces {

  sealed trait TCPiece {
    def symbol: String
  }

  sealed trait PromotableTo extends TCPiece

  sealed trait NonPawn extends TCPiece

  private sealed class PieceS(symb: String) extends TCPiece {
    override def symbol: String = symb
  }

  case object Pawn extends PieceS("")

  case object Knight extends PieceS("N") with PromotableTo with NonPawn

  case object Bishop extends PieceS("B") with PromotableTo with NonPawn

  case object Rook extends PieceS("R") with PromotableTo with NonPawn

  case object Queen extends PieceS("Q") with PromotableTo with NonPawn

  case object King extends PieceS("K") with NonPawn

}
