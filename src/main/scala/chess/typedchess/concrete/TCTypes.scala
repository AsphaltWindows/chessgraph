package chess.typedchess.concrete

import chess.model.Types
import chess.typedchess.internal.Pieces.TCPiece
import chess.typedchess.internal._

object TCTypes extends Types {

  override type PieceType = TCPiece

  override type File = TCFile

  override type Rank = TCRank

  override type Side = SideColor

  override type State = GameState

}
