package chess.indexed2chess

import chess.model.Chess
import chess.indexed2chess.concrete.{ICRules, _}

object Indexed2Chess extends Chess[ICTypes.type] {
  override val t: ICTypes.type = ICTypes

  override type Pos = ICPosition
  override type Mov = ICMove
  override type Not = ICNotation.type
  override type Gam = ICGame
  override type Rul = ICRules.type

  override def newGame: ICGame = new ICGame

  override def rules: ICRules.type = ICRules

}
