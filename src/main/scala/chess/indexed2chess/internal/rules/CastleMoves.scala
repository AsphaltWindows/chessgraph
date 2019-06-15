package chess.indexed2chess.internal.rules

import chess.indexed2chess.concrete.Moves._
import chess.indexed2chess.concrete.ICMove
import chess.indexed2chess.internal.state.{Black, SideColor, White}

object CastleMoves {

  import chess.indexed2chess.concrete.ICTypes._
  import chess.indexed2chess.internal.state.Board._

  val nonThreatSquares: Map[Castle, Seq[Square]] = Map(
    LongCastleWhite -> Seq(E1, D1, C1),
    LongCastleBlack -> Seq(E8, D8, C8),
    ShortCastleWhite -> Seq(E1, F1, G1),
    ShortCastleBlack -> Seq(E8, F8, G8)
  )

  val freeSquares: Map[Castle, Seq[Square]] = Map(
    LongCastleWhite -> Seq(D1, C1, B1),
    LongCastleBlack -> Seq(D8, C8, B8),
    ShortCastleWhite -> Seq(F1, G1),
    ShortCastleBlack -> Seq(F8, G8)
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

  val allMovesFlattened: Seq[ICMove] = allCastleMoves.flatMap(_._2).toSeq

  def castleMoves(side: Side,
                  isThreatenedBy: (Square, Side) => Boolean,
                  isFree: Square => Boolean,
                  longCastle: Boolean,
                  shortCastle: Boolean): Seq[Castle] = {
    allCastleMoves(side)
      .filter { castleMove =>
        nonThreatSquares(castleMove).forall { square =>
          !isThreatenedBy(
            square,
            SideColor.other(side)
          ) &&
            freeSquares(castleMove).forall { square =>
              isFree(square)
            }
        } && (castleMove match {
          case l: LongCastle => longCastle
          case s: ShortCastle => shortCastle
        })
      }
  }
}
