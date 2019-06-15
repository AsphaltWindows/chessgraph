package chess.indexed2chess.internal.rules

import chess.indexed2chess.concrete.ICGame.{AdvanceGame, GameOp, IncrementFiftyMoveCounter, ResetFiftyMoveCounter}
import chess.indexed2chess.concrete.{ICMove, ICPosition}
import chess.indexed2chess.concrete.ICPosition._
import chess.indexed2chess.internal.state.{Black, White}

object MovePositionOps {

  import chess.indexed2chess.concrete.ICTypes._
  import chess.indexed2chess.concrete.Moves._
  import chess.indexed2chess.internal.state.Board._


  val movePositionsOpsMap: Map[ICMove, Seq[PositionOp]] = (CastleMoves.allMovesFlattened ++
    KingMoves.allMovesFlattened ++
    KnightMoves.allMovesFlattened ++
    PawnMoves.allMovesFlattened ++
    BishopMoves.allMovesFlattened ++
    RookMoves.allMovesFlattened ++
    QueenMoves.allMovesFlattened).map(move => move -> moveToOps(move))
    .toMap

  def gameOps(move: ICMove, position: ICPosition): Seq[GameOp] = {
    (move match {
      case _: PawnMove => ResetFiftyMoveCounter
      case _: Capture => ResetFiftyMoveCounter
      case _: ICMove => IncrementFiftyMoveCounter
    }) +:
      Seq[GameOp](AdvanceGame(move, position))
  }


  def moveToOps(move: ICMove): Seq[PositionOp] = {
    move match {
      case c: Castle => castleOps(c)
      case nc: NonCastle => nonCastleOps(nc)
    }
  }


  def castleOps(castle: Castle): Seq[PositionOp] = castle match {
    case LongCastleWhite => Seq(Reposition(A1, D1), Reposition(E1, C1), DisableShortCastle(White), DisableLongCastle(White), ResetEnPassant, FlipMove)
    case LongCastleBlack => Seq(Reposition(A8, D8), Reposition(E8, C8), DisableShortCastle(Black), DisableLongCastle(Black), ResetEnPassant, FlipMove)
    case ShortCastleWhite => Seq(Reposition(H1, F1), Reposition(E1, G1), DisableShortCastle(White), DisableLongCastle(White), ResetEnPassant, FlipMove)
    case ShortCastleBlack => Seq(Reposition(H8, F8), Reposition(E8, G8), DisableShortCastle(Black), DisableLongCastle(Black), ResetEnPassant, FlipMove)
  }

  def nonCastleOps(nonCastle: NonCastle): Seq[PositionOp] = nonCastle match {
    case pawn: PawnMove => pawnOps(pawn)
    case nonPawn: PieceMove => nonPawnOps(nonPawn)
  }

  def pawnOps(pawnMove: PawnMove): Seq[PositionOp] = pawnMove match {
    case en: EnPassant => pawnEnpassantOps(en)
    case double: DoubleAdvance => pawnDoubleAdvanceOps(double)
    case promoCapture: Promotion with Capture => pawnPromoCaptureOps(promoCapture)
    case promoAdvance: Promotion with Advance => pawnPromoAdvanceOps(promoAdvance)
    case advance: Advance => moveOps(advance)
    case capture: Capture => captureOps(capture)
  }

  def nonPawnOps(nonPawn: PieceMove): Seq[PositionOp] = nonPawn match {
    case capture: Capture => captureOps(capture)
    case nc: NonCastle => moveOps(nc)
  }

  def pawnEnpassantOps(enPassant: EnPassant): Seq[PositionOp] = Seq(
    Remove(enPassant.enpassant),
    Reposition(enPassant.from, enPassant.to),
    ResetEnPassant,
    FlipMove
  )

  def pawnDoubleAdvanceOps(move: DoubleAdvance): Seq[PositionOp] = Seq(
    Reposition(move.from, move.to),
    SetEnPassant(move.to),
    FlipMove
  )

  def pawnPromoCaptureOps(p: Promotion with Capture): Seq[PositionOp] = Seq(
    Remove(p.to),
    Reposition(p.from, p.to),
    Replace(pce(p.side, p.promoteTo), p.to),
    ResetEnPassant,
    FlipMove
  ) ++ disableCastling(p)

  def pawnPromoAdvanceOps(p: Promotion with Advance): Seq[PositionOp] = Seq(
    Reposition(p.from, p.to),
    Replace(pce(p.side, p.promoteTo), p.to),
    ResetEnPassant,
    FlipMove
  ) ++ disableCastling(p)

  def moveOps(nc: NonCastle): Seq[PositionOp] = Seq(
    Reposition(nc.from, nc.to),
    ResetEnPassant,
    FlipMove
  ) ++ disableCastling(nc)

  def captureOps(p: Capture): Seq[PositionOp] = Seq(
    Remove(p.to),
    Reposition(p.from, p.to),
    ResetEnPassant,
    FlipMove
  ) ++ disableCastling(p)

  def disableCastling(nonCastle: NonCastle): Seq[PositionOp] = {
    if (nonCastle.to == E1 || nonCastle.from == E1) {
      Seq(
        DisableLongCastle(White),
        DisableShortCastle(White)
      )
    }
    else if (nonCastle.to == E8 || nonCastle.from == E8) {
      Seq(
        DisableLongCastle(Black),
        DisableShortCastle(Black)
      )
    }
    else if (nonCastle.to == A1 || nonCastle.from == A1) {
      Seq(
        DisableLongCastle(White)
      )
    }
    else if (nonCastle.to == A8 || nonCastle.from == A8) {
      Seq(
        DisableLongCastle(Black)
      )
    }
    else if (nonCastle.to == H1 || nonCastle.from == H1) {
      Seq(
        DisableShortCastle(White)
      )
    }
    else if (nonCastle.to == H8 || nonCastle.from == H8) {
      Seq(
        DisableShortCastle(Black)
      )
    }
    else {
      Seq()
    }
  }

}
