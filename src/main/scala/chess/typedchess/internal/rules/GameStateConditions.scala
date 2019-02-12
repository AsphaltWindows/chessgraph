package chess.typedchess.internal.rules

import chess.typedchess.concrete.{TCGame, TCPosition}
import chess.typedchess.internal.state.PieceTypes._
import chess.typedchess.internal.state.Pieces.TCPiece
import chess.typedchess.internal.state._

object GameStateConditions {

  import chess.typedchess.concrete.TCTypes._

  def calculateGameState(game: TCGame): State = {

    if (insufficientMaterial(game) ||
      fiftyMoveRule(game) ||
      threeTimeRepetition(game)) {
      Draw
    }
    else if (LegalMoves.legalNextMovesPositions(game.currentPosition).isEmpty) {

      if (Threats.isKingInCheck(game.currentPosition, game.currentPosition.toMove)) {
        SideColor.other(game.currentPosition.toMove) match {
          case White => WhiteWin
          case Black => BlackWin
        }
      }
      else {
        Draw
      }
    }
    else {
      InProgress
    }
  }

  private def insufficientMaterial(game: TCGame): Boolean = {
    val whitePieces = game.currentPosition.allPieces(White)
    val blackPieces = game.currentPosition.allPieces(Black)

    (whitePieces.size == 1 && blackPieces.size == 1) ||
      (whitePieces.size == 1 &&
        blackPieces.size == 2 &&
        blackPieces.forall { case (_, TCPiece(_, pt)) =>
          pt != Rook && pt != Queen && pt != Pawn
        }
        ) ||
      (blackPieces.size == 1 &&
        whitePieces.size == 2 &&
        whitePieces.forall { case (_, TCPiece(_, pt)) =>
          pt != Rook && pt != Queen && pt != Pawn
        }
        ) ||
      (whitePieces.forall { case (s, TCPiece(_, pt)) =>
        pt == King || (pt == Bishop && Diagonals.lightSquares.contains(s))
      } &&
        blackPieces.forall { case (s, TCPiece(_, pt)) =>
          pt == King || (pt == Bishop && Diagonals.lightSquares.contains(s))
        }
        ) ||
      (whitePieces.forall { case (s, TCPiece(_, pt)) =>
        pt == King || (pt == Bishop && Diagonals.darkSquares.contains(s))
      } &&
        blackPieces.forall { case (s, TCPiece(_, pt)) =>
          pt == King || (pt == Bishop && Diagonals.darkSquares.contains(s))
        }
        )
  }

  private def fiftyMoveRule(game: TCGame): Boolean = {
    game.fiftyMoveRuleCounter == 100
  }

  private def threeTimeRepetition(game: TCGame): Boolean = {
    game.positionFrequency(game.currentPosition) == 3
  }

}
