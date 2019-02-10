package chess.typedchess

import chess.model.Model
import chess.typedchess.concrete._

object TypedChess extends Model[TCTypes.type] {
  override val t: TCTypes.type = TCTypes

  override type Pos = TCPosition
  override type Mov = TCMove
  override type Not = TCNotation.type
  override type Gam = TCGame
  override type Rul = TCRules.type
}
