package chess.typedchess

import chess.model.Chess
import chess.typedchess.concrete.{TCRules, _}

object TypedChess extends Chess[TCTypes.type] {
  override val t: TCTypes.type = TCTypes

  override type Pos = TCPosition
  override type Mov = TCMove
  override type Not = TCNotation.type
  override type Gam = TCGame
  override type Rul = TCRules.type

  override def newGame: TCGame = new TCGame

  override def rules: TCRules.type = TCRules

}
