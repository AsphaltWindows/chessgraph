package chess.typedchess.concrete

import chess.model.Move
import chess.typedchess.internal.Pieces.{NonPawn, Pawn, PromotableTo}
import chess.typedchess.internal._

sealed trait TCMove extends Move[TCTypes.type] {
  override val t: TCTypes.type = TCTypes
  def side: t.Side
}

object Moves {

  val types = TCTypes

  private sealed trait Castle extends TCMove

  private sealed abstract class NonCastle(pce: types.PieceType, fr: types.Square, to: types.Square) extends TCMove

  private sealed trait WhiteMove {
    self: TCMove =>

    override def side: t.Side = White
  }

  private sealed trait BlackMove {
    self: TCMove =>

    override def side: t.Side = Black
  }

  private abstract class PawnMove(fr: types.Square, to: types.Square) extends NonCastle(Pawn, fr, to)

  private sealed abstract class NonPawnMove(pce: NonPawn, fr: types.Square, to: types.Square) extends NonCastle(pce, fr, to)

  private sealed trait Promotion extends TCMove {
    self: PawnMove =>

    def promoteTo: PromotableTo
  }

  private sealed trait Capture extends TCMove {
    self: NonCastle =>
  }

  private sealed trait EnPassant extends PawnMove {
    self: Capture =>

    def enpassant: types.Square
  }

  case object LongCastleWhite extends Castle with WhiteMove
  case object ShortCastleWhite extends Castle with WhiteMove
  case class PieceMoveWhite(piece: NonPawn, from: types.Square, to: types.Square) extends NonCastle(piece, from, to) with WhiteMove
  case class PieceCaptureWhite(piece: NonPawn, from: types.Square, to: types.Square) extends NonCastle(piece, from, to) with Capture with WhiteMove
  case class PawnAdvanceWhite(from: types.Square, to: types.Square) extends PawnMove(from, to) with WhiteMove
  case class PawnCaptureWhite(from: types.Square, to: types.Square) extends PawnMove(from, to) with Capture with WhiteMove
  case class PawnAdvancePromoteWhite(from: types.Square, to: types.Square, promo: PromotableTo) extends PawnMove(from, to) with Promotion with WhiteMove { override def promoteTo: PromotableTo = promo }
  case class PawnCapturePromoteWhite(from: types.Square, to: types.Square, promo: PromotableTo) extends PawnMove(from, to) with Promotion with WhiteMove { override def promoteTo: PromotableTo = promo }
  case class PawnEnPassantWhite(from: types.Square, to: types.Square, enpass: types.Square) extends PawnMove(from, to) with EnPassant with Capture with WhiteMove { override def enpassant: types.Square = enpass}


  case object LongCastleBlack extends Castle with BlackMove
  case object ShortCastleBlack extends Castle with BlackMove
  case class PieceMoveBlack(piece: NonPawn, from: types.Square, to: types.Square) extends NonCastle(piece, from, to) with BlackMove
  case class PieceCaptureBlack(piece: NonPawn, from: types.Square, to: types.Square) extends NonCastle(piece, from, to) with Capture with BlackMove
  case class PawnAdvanceBlack(from: types.Square, to: types.Square) extends PawnMove(from, to) with BlackMove
  case class PawnCaptureBlack(from: types.Square, to: types.Square) extends PawnMove(from, to) with Capture with BlackMove
  case class PawnAdvancePromoteBlack(from: types.Square, to: types.Square, promo: PromotableTo) extends PawnMove(from, to) with Promotion with BlackMove { override def promoteTo: PromotableTo = promo }
  case class PawnCapturePromoteBlack(from: types.Square, to: types.Square, promo: PromotableTo) extends PawnMove(from, to) with Promotion with BlackMove { override def promoteTo: PromotableTo = promo }
  case class PawnEnPassantBlack(from: types.Square, to: types.Square, enpass: types.Square) extends PawnMove(from, to) with EnPassant with Capture with BlackMove { override def enpassant: types.Square = enpass}
}


