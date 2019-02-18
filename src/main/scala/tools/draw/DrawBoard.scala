package tools.draw
import chess.typedchess.concrete.TCPosition
import chess.typedchess.internal.state.{Black, Board, White}
import chess.typedchess.internal.state.Board.TCSquare
import chess.typedchess.internal.state.PieceTypes._
import chess.typedchess.internal.state.Pieces._

object DrawBoard {

  val fileStr = "    a   b   c   d   e   f   g   h"
  val rankSep = "  +---+---+---+---+---+---+---+---+"

  def draw(pos: TCPosition): Unit = {
    val board = Board.allRanks.reverse.map{ r =>
      val rank = Board.allFiles.map{ f =>
        pos.onSquare(TCSquare(f, r)) match {
          case Some(p) =>  p match {
              case TCPiece(White, Pawn) => "\u2659"
              case TCPiece(White, Knight) => "\u2658"
              case TCPiece(White, Bishop) => "\u2657"
              case TCPiece(White, Rook) => "\u2656"
              case TCPiece(White, Queen) => "\u2655"
              case TCPiece(White, King) => "\u2654"
              case TCPiece(Black, Pawn) => "\u265F"
              case TCPiece(Black, Knight) => "\u265E"
              case TCPiece(Black, Bishop) => "\u265D"
              case TCPiece(Black, Rook) => "\u265C"
              case TCPiece(Black, Queen) => "\u265B"
              case TCPiece(Black, King) => "\u265A"
            }
          case None => " "
        }
      }.mkString("| ", " | ", " |")
      s"${r.symbol} " + rank
    }.mkString(s"$rankSep\n", s"\n$rankSep\n", s"\n$rankSep\n$fileStr\n")
    print(board)
  }

}
