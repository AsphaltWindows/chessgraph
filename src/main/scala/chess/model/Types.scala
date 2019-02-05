package chess.model

trait Types {
  type Side
  type PieceType
  type Rank
  type File
  type State

  type Square = (File, Rank)
  type Piece = (Side, PieceType)
}
