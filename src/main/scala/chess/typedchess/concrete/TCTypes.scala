package chess.typedchess.concrete

import chess.model.Types
import chess.typedchess.internal.state.PieceTypes._
import chess.typedchess.internal.state.Pieces.TCPiece
import chess.typedchess.internal.state._

object TCTypes extends Types {

  override type PieceType = TCPieceType

  override type File = TCFile

  override type Rank = TCRank

  override type Side = SideColor

  override type State = GameState

  override type Square = Board.Square

  override type Piece = TCPiece

  override def sq(file: TCFile, rank: TCRank): Square = Board.allSquaresByFileRank(file)(rank)

  override def pce(side: SideColor, pieceType: TCPieceType): Piece = Pieces.pieceMap(side)(pieceType)


}
