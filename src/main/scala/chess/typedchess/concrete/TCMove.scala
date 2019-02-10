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

  sealed abstract class NonPawnMove(pce: NonPawn, f: Square, t: Square) extends NonCastle(pce, f, t)

  sealed trait Promotion extends TCMove {
    self: PawnMove =>

    def promoteTo: PromotableTo
  }

  sealed trait Capture extends TCMove {
    self: NonCastle =>
  }

  sealed trait EnPassant extends PawnMove {
    self: Capture =>

    def enpassant: Square
  }

  case object LongCastleWhite extends Castle with WhiteMove
  case object ShortCastleWhite extends Castle with WhiteMove
  case class PieceMoveWhite(pce: NonPawn, f: Square, t: Square) extends NonCastle(pce, f, t) with WhiteMove
  case class PieceCaptureWhite(pce: NonPawn, f: Square, t: Square) extends NonCastle(pce, f, t) with Capture with WhiteMove
  case class PawnAdvanceWhite(f: Square, t: Square) extends PawnMove(f, t) with WhiteMove
  case class PawnDoubleAdvanceWhite(f: Square, t: Square) extends PawnMove(f, t) with WhiteMove
  case class PawnCaptureWhite(f: Square, t: Square) extends PawnMove(f, t) with Capture with WhiteMove
  case class PawnAdvancePromoteWhite(f: Square, t: Square, promo: PromotableTo) extends PawnMove(f, t) with Promotion with WhiteMove { override def promoteTo: PromotableTo = promo }
  case class PawnCapturePromoteWhite(f: Square, t: Square, promo: PromotableTo) extends PawnMove(f, t) with Promotion with WhiteMove { override def promoteTo: PromotableTo = promo }
  case class PawnEnPassantWhite(f: Square, t: Square, enpass: Square) extends PawnMove(f, t) with EnPassant with Capture with WhiteMove { override def enpassant: Square = enpass}


  case object LongCastleBlack extends Castle with BlackMove
  case object ShortCastleBlack extends Castle with BlackMove
  case class PieceMoveBlack(pce: NonPawn, f: Square, t: Square) extends NonCastle(pce, f, t) with BlackMove
  case class PieceCaptureBlack(pce: NonPawn, f: Square, t: Square) extends NonCastle(pce, f, t) with Capture with BlackMove
  case class PawnAdvanceBlack(f: Square, t: Square) extends PawnMove(f, t) with BlackMove
  case class PawnDoubleAdvanceBlack(f: Square, t: Square) extends PawnMove(f, t) with BlackMove
  case class PawnCaptureBlack(f: Square, t: Square) extends PawnMove(f, t) with Capture with BlackMove
  case class PawnAdvancePromoteBlack(f: Square, t: Square, promo: PromotableTo) extends PawnMove(f, t) with Promotion with BlackMove { override def promoteTo: PromotableTo = promo }
  case class PawnCapturePromoteBlack(f: Square, t: Square, promo: PromotableTo) extends PawnMove(f, t) with Promotion with BlackMove { override def promoteTo: PromotableTo = promo }
  case class PawnEnPassantBlack(f: Square, t: Square, enpass: Square) extends PawnMove(f, t) with EnPassant with Capture with BlackMove { override def enpassant: Square = enpass}
}


