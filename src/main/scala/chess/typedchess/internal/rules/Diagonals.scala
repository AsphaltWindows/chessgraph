package chess.typedchess.internal.rules

object Diagonals {

  import chess.typedchess.concrete.TCTypes._
  import chess.typedchess.internal.state.Board._

  type Diagonal = Set[Square]

  val A1H8 = Set(A1, B2, C3, D4, E5, F6, G7, H8)
  val A2B1 = Set(A2, B1)
  val A2G8 = Set(A2, B3, C4, D5, E6, F7, G8)
  val A3C1 = Set(A3, B2, C1)
  val A3F8 = Set(A3, B4, C5, D6, E7, F8)
  val A4D1 = Set(A4, B3, C2, D1)
  val A4E8 = Set(A4, B5, C6, D7, E8)
  val A5E1 = Set(A5, B4, C3, D2, E1)
  val A5D8 = Set(A5, B6, C7, D8)
  val A6F1 = Set(A6, B5, C4, D3, E2, F1)
  val A6C8 = Set(A6, B7, C8)
  val A7G1 = Set(A7, B6, C5, D4, E3, F2, G1)
  val A7B8 = Set(A7, B8)
  val A8H1 = Set(A8, B7, C6, D5, E4, F3, G2, H1)


  val allDiagonals: Seq[Diagonal] = Seq(
    A1H8,
    A2B1,
    A2G8,
    A3C1,
    A3F8,
    A4D1,
    A4E8,
    A5E1,
    A5D8,
    A6F1,
    A6C8,
    A7G1,
    A7B8,
    A8H1
  )

  val squareToDiagonal: Map[Square, Seq[Diagonal]] = allSquares
    .map { square =>
      square -> allDiagonals.filter { diag =>
        diag.contains(square)
      }
    }
    .toMap

  val diagonalVectors: Map[Square, Seq[Seq[Square]]] = allSquares
    .map { start =>

      val topLeft: Seq[Square] = (1 to 8)
        .foldLeft[(Seq[Square], Option[Square])]((Seq(), Some(start))) {
        case ((acc, None), _) => (acc, None)
        case ((acc, Some(last)), _) =>
          val nextOpt = (last.file - 1)
            .zip((last.rank) + 1)
            .map { case (f, r) => sq(f, r) }
          (nextOpt.toSeq ++ acc, nextOpt.headOption)
      }
        ._1
        .reverse

      val topRight: Seq[Square] = (1 to 8)
        .foldLeft[(Seq[Square], Option[Square])]((Seq(), Some(start))) {
        case ((acc, None), _) => (acc, None)
        case ((acc, Some(last)), _) =>
          val nextOpt = (last.file + 1)
            .zip((last.rank) + 1)
            .map { case (f, r) => sq(f, r) }
          (nextOpt.toSeq ++ acc, nextOpt.headOption)
      }
        ._1
        .reverse

      val bottomLeft: Seq[Square] = (1 to 8)
        .foldLeft[(Seq[Square], Option[Square])]((Seq(), Some(start))) {
        case ((acc, None), _) => (acc, None)
        case ((acc, Some(last)), _) =>
          val nextOpt = (last.file - 1)
            .zip((last.rank) - 1)
            .map { case (f, r) => sq(f, r) }
          (nextOpt.toSeq ++ acc, nextOpt.headOption)
      }
        ._1
        .reverse

      val bottomRight: Seq[Square] = (1 to 8)
        .foldLeft[(Seq[Square], Option[Square])]((Seq(), Some(start))) {
        case ((acc, None), _) => (acc, None)
        case ((acc, Some(last)), _) =>
          val nextOpt = (last.file + 1)
            .zip((last.rank) - 1)
            .map { case (f, r) => sq(f, r) }
          (nextOpt.toSeq ++ acc, nextOpt.headOption)
      }
        ._1
        .reverse

      start -> Seq(topLeft, topRight, bottomLeft, bottomRight)
    }
    .toMap


}
