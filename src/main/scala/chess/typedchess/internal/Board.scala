package chess.typedchess.internal

sealed trait TCFile {
  def symbol: String
}

sealed trait TCRank {
  def symbol: String
}

object Board {

  private sealed class FileS(symb: String) extends TCFile {
    override def symbol: String = symb
  }

  private sealed class RankS(symb: String) extends TCRank {
    override def symbol: String = symb
  }

  case object A extends FileS("A")
  case object B extends FileS("B")
  case object C extends FileS("C")
  case object D extends FileS("D")
  case object E extends FileS("E")
  case object F extends FileS("F")
  case object G extends FileS("G")
  case object H extends FileS("H")

  case object `1` extends RankS("1")
  case object `2` extends RankS("2")
  case object `3` extends RankS("3")
  case object `4` extends RankS("4")
  case object `5` extends RankS("5")
  case object `6` extends RankS("6")
  case object `7` extends RankS("7")
  case object `8` extends RankS("8")
}
