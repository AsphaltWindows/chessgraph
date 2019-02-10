package chess.typedchess.internal.state

object Pieces {

  abstract class TCPiece(val symbol: String)

  sealed trait PromotableTo extends TCPiece

  sealed trait NonPawn extends TCPiece

  sealed trait Linear extends NonPawn

  case object Pawn extends TCPiece("")

  case object Knight extends TCPiece("N") with PromotableTo with NonPawn

  case object Bishop extends TCPiece("B") with PromotableTo with Linear with NonPawn

  case object Rook extends TCPiece("R") with PromotableTo with Linear with NonPawn

  case object Queen extends TCPiece("Q") with PromotableTo with Linear with NonPawn

  case object King extends TCPiece("K") with NonPawn

}
