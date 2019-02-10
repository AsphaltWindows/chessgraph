package chess.typedchess.concrete

import chess.model.Move
import chess.typedchess.internal.state.Pieces.{NonPawn, Pawn, PromotableTo}
import chess.typedchess.internal.state.{Black, White}

sealed trait TCMove extends Move[TCTypes.type] {
  override val types: TCTypes.type = TCTypes
  def side: types.Side
}

object Moves {


  import TCTypes._

  sealed trait Castle extends TCMove

  sealed trait LongCastle extends Castle

  sealed trait ShortCastle extends Castle

  sealed abstract class NonCastle(val piece: PieceType, val from: Square, val to: Square) extends TCMove

  sealed trait WhiteMove {
    self: TCMove =>

    override def side: Side = White
  }

  sealed trait BlackMove {
    self: TCMove =>

    override def side: Side = Black
  }

  sealed abstract class PawnMove(f: Square, t: Square) extends NonCastle(Pawn, f, t)

  sealed trait Advance extends PawnMove

  sealed trait DoubleAdvance extends PawnMove

  sealed abstract class PieceMove(pce: NonPawn, f: Square, t: Square) extends NonCastle(pce, f, t)

  sealed trait Promotion extends PawnMove {
    def promoteTo: PromotableTo
  }

  sealed trait Capture extends NonCastle

  sealed trait EnPassant extends PawnMove {
    self: Capture =>

    def enpassant: Square
  }

  case object LongCastleWhite extends LongCastle with WhiteMove
  case object ShortCastleWhite extends ShortCastle with WhiteMove
  case class PieceMoveWhite(pce: NonPawn, f: Square, t: Square) extends PieceMove(pce, f, t) with WhiteMove
  case class PieceCaptureWhite(pce: NonPawn, f: Square, t: Square) extends PieceMove(pce, f, t) with Capture with WhiteMove
  case class PawnAdvanceWhite(f: Square, t: Square) extends PawnMove(f, t) with Advance with WhiteMove
  case class PawnDoubleAdvanceWhite(f: Square, t: Square) extends PawnMove(f, t) with DoubleAdvance with WhiteMove
  case class PawnCaptureWhite(f: Square, t: Square) extends PawnMove(f, t) with Capture with WhiteMove
  case class PawnAdvancePromoteWhite(f: Square, t: Square, promo: PromotableTo) extends PawnMove(f, t) with Promotion with Advance with WhiteMove { override def promoteTo: PromotableTo = promo }
  case class PawnCapturePromoteWhite(f: Square, t: Square, promo: PromotableTo) extends PawnMove(f, t) with Promotion with Capture with WhiteMove { override def promoteTo: PromotableTo = promo }
  case class PawnEnPassantWhite(f: Square, t: Square, enpass: Square) extends PawnMove(f, t) with EnPassant with Capture with WhiteMove { override def enpassant: Square = enpass}


  case object LongCastleBlack extends LongCastle with BlackMove
  case object ShortCastleBlack extends ShortCastle with BlackMove
  case class PieceMoveBlack(pce: NonPawn, f: Square, t: Square) extends PieceMove(pce, f, t) with BlackMove
  case class PieceCaptureBlack(pce: NonPawn, f: Square, t: Square) extends PieceMove(pce, f, t) with Capture with BlackMove
  case class PawnAdvanceBlack(f: Square, t: Square) extends PawnMove(f, t) with Advance with BlackMove
  case class PawnDoubleAdvanceBlack(f: Square, t: Square) extends PawnMove(f, t) with DoubleAdvance with BlackMove
  case class PawnCaptureBlack(f: Square, t: Square) extends PawnMove(f, t) with Capture with BlackMove
  case class PawnAdvancePromoteBlack(f: Square, t: Square, promo: PromotableTo) extends PawnMove(f, t) with Promotion with Advance with BlackMove { override def promoteTo: PromotableTo = promo }
  case class PawnCapturePromoteBlack(f: Square, t: Square, promo: PromotableTo) extends PawnMove(f, t) with Promotion with Capture with BlackMove { override def promoteTo: PromotableTo = promo }
  case class PawnEnPassantBlack(f: Square, t: Square, enpass: Square) extends PawnMove(f, t) with EnPassant with Capture with BlackMove { override def enpassant: Square = enpass}
}


