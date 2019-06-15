package tools.draw
import chess.indexed2chess.concrete.ICPosition
import chess.typedchess.concrete.TCPosition

object DrawBoard {

  val fileStr = "    a   b   c   d   e   f   g   h"
  val rankSep = "  +---+---+---+---+---+---+---+---+"

  def draw(pos: TCPosition): String = {
    import chess.typedchess.internal.state.{Black, Board, White}
    import chess.typedchess.internal.state.Board.TCSquare
    import chess.typedchess.internal.state.PieceTypes._
    import chess.typedchess.internal.state.Pieces._
    Board.allRanks.reverse.map{ r =>
      val rank = Board.allFiles.map{ f =>
        pos.onSquare(TCSquare(f, r)) match {
          case Some(p) =>  p match {
              case TCPiece(Black, Pawn) => "\u2659"
              case TCPiece(Black, Knight) => "\u2658"
              case TCPiece(Black, Bishop) => "\u2657"
              case TCPiece(Black, Rook) => "\u2656"
              case TCPiece(Black, Queen) => "\u2655"
              case TCPiece(Black, King) => "\u2654"
              case TCPiece(White, Pawn) => "\u265F"
              case TCPiece(White, Knight) => "\u265E"
              case TCPiece(White, Bishop) => "\u265D"
              case TCPiece(White, Rook) => "\u265C"
              case TCPiece(White, Queen) => "\u265B"
              case TCPiece(White, King) => "\u265A"
            }
          case None => " "
        }
      }.mkString("| ", " | ", " |")
      s"${r.symbol} " + rank
    }.mkString(s"$rankSep\n", s"\n$rankSep\n", s"\n$rankSep\n$fileStr\n")
  }

  def draw(pos: ICPosition): String = {
    import chess.indexed2chess.internal.state.{Black, Board, White}
    import chess.indexed2chess.internal.state.Board.ICSquare
    import chess.indexed2chess.internal.state.PieceTypes.{Bishop, King, Knight, Pawn, Queen, Rook}
    import chess.indexed2chess.internal.state.Pieces.ICPiece
    Board.allRanks.reverse.map{ r =>
      val rank = Board.allFiles.map{ f =>
        pos.onSquare(ICSquare(f, r)) match {
          case Some(p) =>  p match {
            case ICPiece(Black, Pawn) => "\u2659"
            case ICPiece(Black, Knight) => "\u2658"
            case ICPiece(Black, Bishop) => "\u2657"
            case ICPiece(Black, Rook) => "\u2656"
            case ICPiece(Black, Queen) => "\u2655"
            case ICPiece(Black, King) => "\u2654"
            case ICPiece(White, Pawn) => "\u265F"
            case ICPiece(White, Knight) => "\u265E"
            case ICPiece(White, Bishop) => "\u265D"
            case ICPiece(White, Rook) => "\u265C"
            case ICPiece(White, Queen) => "\u265B"
            case ICPiece(White, King) => "\u265A"
          }
          case None => " "
        }
      }.mkString("| ", " | ", " |")
      s"${r.symbol} " + rank
    }.mkString(s"$rankSep\n", s"\n$rankSep\n", s"\n$rankSep\n$fileStr\n")
  }

}
