package chess.indexed2chess.internal.state

object PieceTypes {

  abstract class ICPieceType(val symbol: String)

  sealed trait PromotableTo extends ICPieceType

  sealed trait NonPawn extends ICPieceType

  sealed trait Linear extends NonPawn

  case object Pawn extends ICPieceType("P")

  case object Knight extends ICPieceType("N") with PromotableTo with NonPawn

  case object Bishop extends ICPieceType("B") with PromotableTo with Linear with NonPawn

  case object Rook extends ICPieceType("R") with PromotableTo with Linear with NonPawn

  case object Queen extends ICPieceType("Q") with PromotableTo with Linear with NonPawn

  case object King extends ICPieceType("K") with NonPawn

}
