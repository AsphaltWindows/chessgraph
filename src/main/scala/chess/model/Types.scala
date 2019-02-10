package chess.model

trait Types {
  type Side
  type PieceType
  type Rank
  type File
  type State
  type Square
  type Piece

  def sq(file: File, rank: Rank): Square
  def pce(side: Side, pieceType: PieceType): Piece
}
