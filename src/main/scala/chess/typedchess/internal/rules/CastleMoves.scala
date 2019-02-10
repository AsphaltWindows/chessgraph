package chess.typedchess.internal.rules

import chess.typedchess.concrete.Moves._
import chess.typedchess.concrete.TCMove
import chess.typedchess.internal.state.{Black, SideColor, White}

object CastleMoves {

  import chess.typedchess.concrete.TCTypes._
  import chess.typedchess.internal.state.Board._

  val nonThreatSquares: Map[Castle, Seq[Square]] = Map(
    LongCastleWhite -> Seq(E1, D1),
    LongCastleBlack -> Seq(E8, D8),
    ShortCastleWhite -> Seq(E1, F1),
    ShortCastleBlack ->  Seq(E8, F8)
  )

  val allCastleMoves: Map[Side, Seq[Castle]] = Map(
    White -> Seq(
      LongCastleWhite,
      ShortCastleWhite
    ),
    Black -> Seq(
      LongCastleBlack,
      ShortCastleBlack
    )
  )

  val allMovesFlattened: Seq[TCMove] = allCastleMoves.flatMap(_._2).toSeq

  def castleMoves(side: Side,
                  isThreatenedBy: (Square, Side) => Boolean,
                  isFree: Square => Boolean,
                  longCastleMap: Map[Side, Boolean],
                  shortCastleMap: Map[Side, Boolean]): Seq[Castle] = {
    allCastleMoves(side)
      .filter { castleMove =>
        nonThreatSquares(castleMove).forall { square =>
          ! isThreatenedBy(
            square,
            SideColor.other(side)
          ) && isFree(square)
        } && (castleMove match {
          case l: LongCastle => longCastleMap(side)
          case s: ShortCastle => shortCastleMap(side)
        })
      }
  }
}
