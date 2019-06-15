package chess.indexed2chess.concrete

import chess.model.Types
import chess.indexed2chess.internal.state.PieceTypes._
import chess.indexed2chess.internal.state.Pieces.ICPiece
import chess.indexed2chess.internal.state._

object ICTypes extends Types {

  override type PieceType = ICPieceType

  override type File = ICFile

  override type Rank = ICRank

  override type Side = SideColor

  override type State = GameState

  override type Square = Board.Square

  override type Piece = ICPiece

  override def sq(file: ICFile, rank: ICRank): Square = Board.allSquaresByFileRank(file)(rank)

  override def pce(side: SideColor, pieceType: ICPieceType): Piece = Pieces.pieceMap(side)(pieceType)


}
