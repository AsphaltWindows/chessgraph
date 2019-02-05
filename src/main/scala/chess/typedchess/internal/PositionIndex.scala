package chess.typedchess.internal

import chess.typedchess.concrete.TCTypes
import chess.typedchess.internal.Pieces._

import scala.collection.mutable
import scala.collection.mutable.{Map => MutMap}
import scala.collection.mutable.{Set => MutSet}

abstract class PositionIndex() {

  import TCTypes._
  import Board._

  val boardMap: MutMap[Square, Piece]
  val pieceMap: MutMap[Piece, MutSet[Square]]
  val blackSquares: MutSet[Square]
  val whiteSquares: MutSet[Square]
  var whiteKing: Square = (E, `1`)
  var blackKing: Square = (E, `8`)
  var whiteLongCastle: Boolean = true
  var whiteShortCastle: Boolean = true
  var blackLongCastle: Boolean = true
  var blackShortCastle: Boolean = true
  var sideToMove: Side = White

}

object PositionIndex {

  import TCTypes._
  import Board._

  private val initialBoard: MutMap[Square, Piece] = MutMap(
    (A, `1`) -> (White, Rook),
    (B, `1`) -> (White, Knight),
    (C, `1`) -> (White, Bishop),
    (D, `1`) -> (White, Queen),
    (E, `1`) -> (White, King),
    (F, `1`) -> (White, Bishop),
    (G, `1`) -> (White, Knight),
    (H, `1`) -> (White, Rook),
    (A, `2`) -> (White, Pawn),
    (B, `2`) -> (White, Pawn),
    (C, `2`) -> (White, Pawn),
    (D, `2`) -> (White, Pawn),
    (E, `2`) -> (White, Pawn),
    (F, `2`) -> (White, Pawn),
    (G, `2`) -> (White, Pawn),
    (H, `2`) -> (White, Pawn),
    (A, `7`) -> (White, Pawn),
    (B, `7`) -> (White, Pawn),
    (C, `7`) -> (White, Pawn),
    (D, `7`) -> (White, Pawn),
    (E, `7`) -> (White, Pawn),
    (F, `7`) -> (White, Pawn),
    (G, `7`) -> (White, Pawn),
    (H, `7`) -> (White, Pawn),
    (A, `8`) -> (White, Rook),
    (B, `8`) -> (White, Knight),
    (C, `8`) -> (White, Bishop),
    (D, `8`) -> (White, Queen),
    (E, `8`) -> (White, King),
    (F, `8`) -> (White, Bishop),
    (G, `8`) -> (White, Knight),
    (H, `8`) -> (White, Rook)
  )

  private val initialPiece: MutMap[Piece, MutSet[Square]] = MutMap(
    (White, Rook) -> MutSet((A, `1`), (H, `1`)),
    (White, Knight) -> MutSet((B, `1`), (G, `1`)),
    (White, Bishop) -> MutSet((C, `1`), (F, `1`)),
    (White, Queen) -> MutSet((D, `1`)),
    (White, King) -> MutSet((E, `1`)),
    (White, Pawn) -> MutSet((A, `2`), (B, `2`),(C, `2`), (D, `2`),(E, `2`), (F, `2`),(G, `2`), (H, `2`)),
    (Black, Rook) -> MutSet((A, `8`), (H, `8`)),
    (Black, Knight) -> MutSet((B, `8`), (G, `8`)),
    (Black, Bishop) -> MutSet((C, `8`), (F, `8`)),
    (Black, Queen) -> MutSet((D, `8`)),
    (Black, King) -> MutSet((B, `8`)),
    (Black, Pawn) -> MutSet((A, `7`),(B, `7`),(C, `7`),(D, `7`),(E, `7`),(F, `7`),(G, `7`),(H, `7`))
  )

  def init(): PositionIndex = new PositionIndex {
    override val boardMap: mutable.Map[(TCFile, TCRank), (SideColor, TCPiece)] = MutMap() ++ initialBoard
    override val pieceMap: mutable.Map[(SideColor, TCPiece), mutable.Set[(TCFile, TCRank)]] = MutMap() ++ initialPiece
    override val blackSquares: mutable.Set[(TCFile, TCRank)] = MutSet() ++ initialBoard.filter{_._2._1 == Black}.keySet
    override val whiteSquares: mutable.Set[(TCFile, TCRank)] = MutSet() ++ initialBoard.filter{_._2._1 == White}.keySet
  }
}