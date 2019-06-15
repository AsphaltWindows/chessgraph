package chess.indexed2chess.internal.rules

import chess.indexed2chess.concrete.{ICGame, ICPosition}
import chess.indexed2chess.internal.state.PieceTypes._
import chess.indexed2chess.internal.state.Pieces.ICPiece
import chess.indexed2chess.internal.state._

object GameStateConditions {

  import chess.indexed2chess.concrete.ICTypes._

  def calculateGameState(game: ICGame): State = {

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

  private def insufficientMaterial(game: ICGame): Boolean = {
    val whitePieces = game.currentPosition.allPieces(White)
    val blackPieces = game.currentPosition.allPieces(Black)

    (whitePieces.size == 1 && blackPieces.size == 1) ||
      (whitePieces.size == 1 &&
        blackPieces.size == 2 &&
        blackPieces.forall { case (_, ICPiece(_, pt)) =>
          pt != Rook && pt != Queen && pt != Pawn
        }
        ) ||
      (blackPieces.size == 1 &&
        whitePieces.size == 2 &&
        whitePieces.forall { case (_, ICPiece(_, pt)) =>
          pt != Rook && pt != Queen && pt != Pawn
        }
        ) ||
      (whitePieces.forall { case (s, ICPiece(_, pt)) =>
        pt == King || (pt == Bishop && Diagonals.lightSquares.contains(s))
      } &&
        blackPieces.forall { case (s, ICPiece(_, pt)) =>
          pt == King || (pt == Bishop && Diagonals.lightSquares.contains(s))
        }
        ) ||
      (whitePieces.forall { case (s, ICPiece(_, pt)) =>
        pt == King || (pt == Bishop && Diagonals.darkSquares.contains(s))
      } &&
        blackPieces.forall { case (s, ICPiece(_, pt)) =>
          pt == King || (pt == Bishop && Diagonals.darkSquares.contains(s))
        }
        )
  }

  private def fiftyMoveRule(game: ICGame): Boolean = {
    game.fiftyMoveRuleCounter == 100
  }

  private def threeTimeRepetition(game: ICGame): Boolean = {
    game.positionFrequency(game.currentPosition) == 3
  }

}
