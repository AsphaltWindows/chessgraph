package chess.indexed2chess.internal.rules

object Diagonals {

  import chess.indexed2chess.concrete.ICTypes._
  import chess.indexed2chess.internal.state.Board._

  type Diagonal = Set[Square]

  val A1H8 = Set(A1, B2, C3, D4, E5, F6, G7, H8)
  val A2B1 = Set(A2, B1)
  val A2G8 = Set(A2, B3, C4, D5, E6, F7, G8)
  val B1H7 = Set(B1, C2, D3, E4, F5, G6, H7)
  val G8H7 = Set(G8, H7)
  val A3C1 = Set(A3, B2, C1)
  val A3F8 = Set(A3, B4, C5, D6, E7, F8)
  val C1H6 = Set(C1, D2, E3, F4, G5, H6)
  val F8H6 = Set(F8, G7, H6)
  val A4D1 = Set(A4, B3, C2, D1)
  val A4E8 = Set(A4, B5, C6, D7, E8)
  val D1H5 = Set(D1, E2, F3, G4, H5)
  val E8H5 = Set(E8, F7, G6, H5)
  val A5E1 = Set(A5, B4, C3, D2, E1)
  val A5D8 = Set(A5, B6, C7, D8)
  val E1H4 = Set(E1, F2, G3, H4)
  val D8H4 = Set(D8, E7, F6, G5, H4)
  val A6F1 = Set(A6, B5, C4, D3, E2, F1)
  val A6C8 = Set(A6, B7, C8)
  val F1H3 = Set(F1, G2, H3)
  val C8H3 = Set(C8, D7, E6, F5, G4, H3)
  val A7G1 = Set(A7, B6, C5, D4, E3, F2, G1)
  val A7B8 = Set(A7, B8)
  val G1H2 = Set(G1, H2)
  val B8H2 = Set(B8, C7, D6, E5, F4, G3, H2)
  val A8H1 = Set(A8, B7, C6, D5, E4, F3, G2, H1)


  val allDiagonals: Seq[Diagonal] = Seq(
    A1H8,
    A2B1,
    A2G8,
    B1H7,
    G8H7,
    A3C1,
    A3F8,
    C1H6,
    F8H6,
    A4D1,
    A4E8,
    D1H5,
    E8H5,
    A5E1,
    A5D8,
    E1H4,
    D8H4,
    A6F1,
    A6C8,
    F1H3,
    C8H3,
    A7G1,
    A7B8,
    G1H2,
    B8H2,
    A8H1
  )

  val darkSquares: Set[Square] = A1H8 ++ A3C1 ++ A3F8 ++ C1H6 ++ F8H6 ++ A5E1 ++ A5D8 ++ E1H4 ++ D8H4 ++ A7G1 ++ A7B8 ++ G1H2 ++ B8H2
  val lightSquares: Set[Square] = A2B1 ++ A2G8 ++ B1H7 ++ G8H7 ++ A4D1 ++ A4E8 ++ D1H5 ++ E8H5 ++ A6F1 ++ A6C8 ++ F1H3 ++ C8H3 ++ A8H1

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
