package adapters.concrete

import adapters.internal.TCInts
import adapters.model.Adapter
import chess.typedchess.TypedChess
import chess.typedchess.concrete.{TCPosition, TCTypes}
import chess.typedchess.internal.state.{Black, Board, White}
import org.platanios.tensorflow.api.Tensor


object PureBinaryAdapter extends Adapter[TCTypes.type, TypedChess.type, Tensor[Boolean]] {
  override val types: TCTypes.type = TCTypes
  override val chess: TypedChess.type = TypedChess

  override def positionAsInput(position: TCPosition): Tensor[Boolean] = {
    Tensor(
      Board
        .allSquares
        .toArray
        .flatMap { square =>
          TCInts.pieceToArray(position.onSquare(square))
        } ++ Array(
        position.toMove match { case White => 1 case Black => 0 },
        if (position.shortCastleMap(White)) 1 else 0,
        if (position.shortCastleMap(Black)) 1 else 0,
        if (position.longCastleMap(White)) 1 else 0,
        if (position.longCastleMap(Black)) 1 else 0
      ).map(_.toByte)
    ).toBoolean
  }
}
