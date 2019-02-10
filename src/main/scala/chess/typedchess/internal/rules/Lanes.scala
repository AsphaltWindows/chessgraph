package chess.typedchess.internal.rules

object Lanes {

  import chess.typedchess.internal.state.Board._
  import chess.typedchess.concrete.TCTypes._

  type Lane = Set[Square]

  val AFile: Lane = allSquaresByFileRank(A).values.toSet
  val BFile: Lane = allSquaresByFileRank(B).values.toSet
  val CFile: Lane = allSquaresByFileRank(C).values.toSet
  val DFile: Lane = allSquaresByFileRank(D).values.toSet
  val EFile: Lane = allSquaresByFileRank(E).values.toSet
  val FFile: Lane = allSquaresByFileRank(F).values.toSet
  val GFile: Lane = allSquaresByFileRank(G).values.toSet
  val HFile: Lane = allSquaresByFileRank(H).values.toSet

  val Rank1: Lane = allSquaresByRankFile(`1`).values.toSet
  val Rank2: Lane = allSquaresByRankFile(`2`).values.toSet
  val Rank3: Lane = allSquaresByRankFile(`3`).values.toSet
  val Rank4: Lane = allSquaresByRankFile(`4`).values.toSet
  val Rank5: Lane = allSquaresByRankFile(`5`).values.toSet
  val Rank6: Lane = allSquaresByRankFile(`6`).values.toSet
  val Rank7: Lane = allSquaresByRankFile(`7`).values.toSet
  val Rank8: Lane = allSquaresByRankFile(`8`).values.toSet

  val allLanes: Seq[Lane] = Seq(
    AFile,
    BFile,
    CFile,
    DFile,
    EFile,
    FFile,
    GFile,
    HFile,
    Rank1,
    Rank2,
    Rank3,
    Rank4,
    Rank5,
    Rank6,
    Rank7,
    Rank8
  )

  val squareToLane: Map[Square, Seq[Lane]] = allSquares
    .map { square =>
      square -> allLanes.filter { lane =>
        lane.contains(square)
      }
    }
    .toMap


  val laneVectors: Map[Square, Seq[Seq[Square]]] = allSquares
    .map { start =>

      val top: Seq[Square] = (1 to 8)
        .foldLeft[(Seq[Square], Option[Square])]((Seq(), Some(start))) {
        case ((acc, None), _) => (acc, None)
        case ((acc, Some(last)), _) =>
          val nextOpt = (last.rank + 1)
            .map { r => sq(last.file, r) }
          (nextOpt.toSeq ++ acc, nextOpt)
      }
        ._1
        .reverse

      val bottom: Seq[Square] = (1 to 8)
        .foldLeft[(Seq[Square], Option[Square])]((Seq(), Some(start))) {
        case ((acc, None), _) => (acc, None)
        case ((acc, Some(last)), _) =>
          val nextOpt = (last.rank - 1)
            .map { r => sq(last.file, r) }
          (nextOpt.toSeq ++ acc, nextOpt)
      }
        ._1
        .reverse

      val left: Seq[Square] = (1 to 8)
        .foldLeft[(Seq[Square], Option[Square])]((Seq(), Some(start))) {
        case ((acc, None), _) => (acc, None)
        case ((acc, Some(last)), _) =>
          val nextOpt = (last.file - 1)
            .map { f => sq(f, last.rank) }
          (nextOpt.toSeq ++ acc, nextOpt)
      }
        ._1
        .reverse

      val right: Seq[Square] = (1 to 8)
        .foldLeft[(Seq[Square], Option[Square])]((Seq(), Some(start))) {
        case ((acc, None), _) => (acc, None)
        case ((acc, Some(last)), _) =>
          val nextOpt = (last.file + 1)
            .map { f => sq(f, last.rank) }
          (nextOpt.toSeq ++ acc, nextOpt)
      }
        ._1
        .reverse

      start -> Seq(top, bottom, left, right)
    }
    .toMap


}
