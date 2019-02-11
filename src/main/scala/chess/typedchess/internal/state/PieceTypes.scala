package chess.typedchess.internal.state

object PieceTypes {

  abstract class TCPieceType(val symbol: String)

  sealed trait PromotableTo extends TCPieceType

  sealed trait NonPawn extends TCPieceType

  sealed trait Linear extends NonPawn

  case object Pawn extends TCPieceType("")

  case object Knight extends TCPieceType("N") with PromotableTo with NonPawn

  case object Bishop extends TCPieceType("B") with PromotableTo with Linear with NonPawn

  case object Rook extends TCPieceType("R") with PromotableTo with Linear with NonPawn

  case object Queen extends TCPieceType("Q") with PromotableTo with Linear with NonPawn

  case object King extends TCPieceType("K") with NonPawn

}
