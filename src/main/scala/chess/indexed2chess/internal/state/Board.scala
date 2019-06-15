package chess.indexed2chess.internal.state

sealed trait ICFile {
  def symbol: String

  def +(forward: Int): Option[ICFile] = Board.fileAdditionMap(this).get(forward)

  def -(back: Int): Option[ICFile] = Board.fileSubtractionMap(this).get(back)
}

sealed trait ICRank {
  def symbol: String

  def +(forward: Int): Option[ICRank] = Board.rankAdditionMap(this).get(forward)

  def -(back: Int): Option[ICRank] = Board.rankSubtractionMap(this).get(back)
}

object Board {

  type Square = ICSquare

  sealed case class ICSquare(file: ICFile, rank: ICRank)

  sealed class FileS(symb: String) extends ICFile {
    override def symbol: String = symb
  }

  sealed class RankS(symb: String) extends ICRank {
    override def symbol: String = symb
  }

  case object A extends FileS("a")

  case object B extends FileS("b")

  case object C extends FileS("c")

  case object D extends FileS("d")

  case object E extends FileS("e")

  case object F extends FileS("f")

  case object G extends FileS("g")

  case object H extends FileS("h")

  case object `1` extends RankS("1")

  case object `2` extends RankS("2")

  case object `3` extends RankS("3")

  case object `4` extends RankS("4")

  case object `5` extends RankS("5")

  case object `6` extends RankS("6")

  case object `7` extends RankS("7")

  case object `8` extends RankS("8")

  val fileAdditionMap: Map[ICFile, Map[Int, ICFile]] = Map(
    A -> Map(
      1 -> B,
      2 -> C,
      3 -> D,
      4 -> E,
      5 -> F,
      6 -> G,
      7 -> H
    ),
    B -> Map(
      1 -> C,
      2 -> D,
      3 -> E,
      4 -> F,
      5 -> G,
      6 -> H
    ),
    C -> Map(
      1 -> D,
      2 -> E,
      3 -> F,
      4 -> G,
      5 -> H
    ),
    D -> Map(
      1 -> E,
      2 -> F,
      3 -> G,
      4 -> H
    ),
    E -> Map(
      1 -> F,
      2 -> G,
      3 -> H
    ),
    F -> Map(
      1 -> G,
      2 -> H
    ),
    G -> Map(
      1 -> H
    ),
    H -> Map()
  )

  val fileSubtractionMap: Map[ICFile, Map[Int, ICFile]] = Map(
    A -> Map(),
    B -> Map(
      1 -> A,
    ),
    C -> Map(
      1 -> B,
      2 -> A
    ),
    D -> Map(
      1 -> C,
      2 -> B,
      3 -> A
    ),
    E -> Map(
      1 -> D,
      2 -> C,
      3 -> B,
      4 -> A
    ),
    F -> Map(
      1 -> E,
      2 -> D,
      3 -> C,
      4 -> B,
      5 -> A
    ),
    G -> Map(
      1 -> F,
      2 -> E,
      3 -> D,
      4 -> C,
      5 -> B,
      6 -> A
    ),
    H -> Map(
      1 -> G,
      2 -> F,
      3 -> E,
      4 -> D,
      5 -> C,
      6 -> B,
      7 -> A
    )
  )

  val rankAdditionMap: Map[ICRank, Map[Int, ICRank]] = Map(
    `1` -> Map(
      1 -> `2`,
      2 -> `3`,
      3 -> `4`,
      4 -> `5`,
      5 -> `6`,
      6 -> `7`,
      7 -> `8`
    ),
    `2` -> Map(
      1 -> `3`,
      2 -> `4`,
      3 -> `5`,
      4 -> `6`,
      5 -> `7`,
      6 -> `8`
    ),
    `3` -> Map(
      1 -> `4`,
      2 -> `5`,
      3 -> `6`,
      4 -> `7`,
      5 -> `8`
    ),
    `4` -> Map(
      1 -> `5`,
      2 -> `6`,
      3 -> `7`,
      4 -> `8`
    ),
    `5` -> Map(
      1 -> `6`,
      2 -> `7`,
      3 -> `8`
    ),
    `6` -> Map(
      1 -> `7`,
      2 -> `8`
    ),
    `7` -> Map(
      1 -> `8`
    ),
    `8` -> Map()
  )

  val rankSubtractionMap: Map[ICRank, Map[Int, ICRank]] = Map(
    `1` -> Map(),
    `2` -> Map(
      1 -> `1`,
    ),
    `3` -> Map(
      1 -> `2`,
      2 -> `1`
    ),
    `4` -> Map(
      1 -> `3`,
      2 -> `2`,
      3 -> `1`
    ),
    `5` -> Map(
      1 -> `4`,
      2 -> `3`,
      3 -> `2`,
      4 -> `1`
    ),
    `6` -> Map(
      1 -> `5`,
      2 -> `4`,
      3 -> `3`,
      4 -> `2`,
      5 -> `1`
    ),
    `7` -> Map(
      1 -> `6`,
      2 -> `5`,
      3 -> `4`,
      4 -> `3`,
      5 -> `2`,
      6 -> `1`
    ),
    `8` -> Map(
      1 -> `7`,
      2 -> `6`,
      3 -> `5`,
      4 -> `4`,
      5 -> `3`,
      6 -> `2`,
      6 -> `1`
    )
  )

  val A1 = ICSquare(A, `1`)
  val A2 = ICSquare(A, `2`)
  val A3 = ICSquare(A, `3`)
  val A4 = ICSquare(A, `4`)
  val A5 = ICSquare(A, `5`)
  val A6 = ICSquare(A, `6`)
  val A7 = ICSquare(A, `7`)
  val A8 = ICSquare(A, `8`)

  val B1 = ICSquare(B, `1`)
  val B2 = ICSquare(B, `2`)
  val B3 = ICSquare(B, `3`)
  val B4 = ICSquare(B, `4`)
  val B5 = ICSquare(B, `5`)
  val B6 = ICSquare(B, `6`)
  val B7 = ICSquare(B, `7`)
  val B8 = ICSquare(B, `8`)

  val C1 = ICSquare(C, `1`)
  val C2 = ICSquare(C, `2`)
  val C3 = ICSquare(C, `3`)
  val C4 = ICSquare(C, `4`)
  val C5 = ICSquare(C, `5`)
  val C6 = ICSquare(C, `6`)
  val C7 = ICSquare(C, `7`)
  val C8 = ICSquare(C, `8`)

  val D1 = ICSquare(D, `1`)
  val D2 = ICSquare(D, `2`)
  val D3 = ICSquare(D, `3`)
  val D4 = ICSquare(D, `4`)
  val D5 = ICSquare(D, `5`)
  val D6 = ICSquare(D, `6`)
  val D7 = ICSquare(D, `7`)
  val D8 = ICSquare(D, `8`)


  val E1 = ICSquare(E, `1`)
  val E2 = ICSquare(E, `2`)
  val E3 = ICSquare(E, `3`)
  val E4 = ICSquare(E, `4`)
  val E5 = ICSquare(E, `5`)
  val E6 = ICSquare(E, `6`)
  val E7 = ICSquare(E, `7`)
  val E8 = ICSquare(E, `8`)


  val F1 = ICSquare(F, `1`)
  val F2 = ICSquare(F, `2`)
  val F3 = ICSquare(F, `3`)
  val F4 = ICSquare(F, `4`)
  val F5 = ICSquare(F, `5`)
  val F6 = ICSquare(F, `6`)
  val F7 = ICSquare(F, `7`)
  val F8 = ICSquare(F, `8`)


  val G1 = ICSquare(G, `1`)
  val G2 = ICSquare(G, `2`)
  val G3 = ICSquare(G, `3`)
  val G4 = ICSquare(G, `4`)
  val G5 = ICSquare(G, `5`)
  val G6 = ICSquare(G, `6`)
  val G7 = ICSquare(G, `7`)
  val G8 = ICSquare(G, `8`)

  val H1 = ICSquare(H, `1`)
  val H2 = ICSquare(H, `2`)
  val H3 = ICSquare(H, `3`)
  val H4 = ICSquare(H, `4`)
  val H5 = ICSquare(H, `5`)
  val H6 = ICSquare(H, `6`)
  val H7 = ICSquare(H, `7`)
  val H8 = ICSquare(H, `8`)

  val allRanks: Seq[ICRank] = Seq(`1`,
    `2`,
    `3`,
    `4`,
    `5`,
    `6`,
    `7`,
    `8`
  )

  val allFiles: Seq[ICFile] = Seq(
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H
  )


  val allSquaresByFileRank: Map[ICFile, Map[ICRank, Square]] = Map(
    A -> Map(
      `1` -> A1,
      `2` -> A2,
      `3` -> A3,
      `4` -> A4,
      `5` -> A5,
      `6` -> A6,
      `7` -> A7,
      `8` -> A8
    ),
    B -> Map(
      `1` -> B1,
      `2` -> B2,
      `3` -> B3,
      `4` -> B4,
      `5` -> B5,
      `6` -> B6,
      `7` -> B7,
      `8` -> B8
    ),
    C -> Map(
      `1` -> C1,
      `2` -> C2,
      `3` -> C3,
      `4` -> C4,
      `5` -> C5,
      `6` -> C6,
      `7` -> C7,
      `8` -> C8
    ),
    D -> Map(
      `1` -> D1,
      `2` -> D2,
      `3` -> D3,
      `4` -> D4,
      `5` -> D5,
      `6` -> D6,
      `7` -> D7,
      `8` -> D8
    ),
    E -> Map(
      `1` -> E1,
      `2` -> E2,
      `3` -> E3,
      `4` -> E4,
      `5` -> E5,
      `6` -> E6,
      `7` -> E7,
      `8` -> E8
    ),
    F -> Map(
      `1` -> F1,
      `2` -> F2,
      `3` -> F3,
      `4` -> F4,
      `5` -> F5,
      `6` -> F6,
      `7` -> F7,
      `8` -> F8
    ),
    G -> Map(
      `1` -> G1,
      `2` -> G2,
      `3` -> G3,
      `4` -> G4,
      `5` -> G5,
      `6` -> G6,
      `7` -> G7,
      `8` -> G8
    ),
    H -> Map(
      `1` -> H1,
      `2` -> H2,
      `3` -> H3,
      `4` -> H4,
      `5` -> H5,
      `6` -> H6,
      `7` -> H7,
      `8` -> H8
    )
  )

  val allSquaresByRankFile: Map[ICRank, Map[ICFile, Square]] = Map(
    `1` -> Map(
      A -> A1,
      B -> B1,
      C -> C1,
      D -> D1,
      E -> E1,
      F -> F1,
      G -> G1,
      H -> H1
    ),
    `2` -> Map(
      A -> A2,
      B -> B2,
      C -> C2,
      D -> D2,
      E -> E2,
      F -> F2,
      G -> G2,
      H -> H2
    ),
    `3` -> Map(
      A -> A3,
      B -> B3,
      C -> C3,
      D -> D3,
      E -> E3,
      F -> F3,
      G -> G3,
      H -> H3
    ),
    `4` -> Map(
      A -> A4,
      B -> B4,
      C -> C4,
      D -> D4,
      E -> E4,
      F -> F4,
      G -> G4,
      H -> H4
    ),
    `5` -> Map(
      A -> A5,
      B -> B5,
      C -> C5,
      D -> D5,
      E -> E5,
      F -> F5,
      G -> G5,
      H -> H5
    ),
    `6` -> Map(
      A -> A6,
      B -> B6,
      C -> C6,
      D -> D6,
      E -> E6,
      F -> F6,
      G -> G6,
      H -> H6
    ),
    `7` -> Map(
      A -> A7,
      B -> B7,
      C -> C7,
      D -> D7,
      E -> E7,
      F -> F7,
      G -> G7,
      H -> H7
    ),
    `8` -> Map(
      A -> A8,
      B -> B8,
      C -> C8,
      D -> D8,
      E -> E8,
      F -> F8,
      G -> G8,
      H -> H8
    )
  )

  val allSquares: Seq[Square] = Seq(
    A1, A2, A3, A4, A5, A6, A7, A8,
    B1, B2, B3, B4, B5, B6, B7, B8,
    C1, C2, C3, C4, C5, C6, C7, C8,
    D1, D2, D3, D4, D5, D6, D7, D8,
    E1, E2, E3, E4, E5, E6, E7, E8,
    F1, F2, F3, F4, F5, F6, F7, F8,
    G1, G2, G3, G4, G5, G6, G7, G8,
    H1, H2, H3, H4, H5, H6, H7, H8
  )

  val squareStrings: Map[Square, String] = allSquares
    .map { square =>
      square -> (square.file.symbol + square.rank.symbol)
    }
    .toMap

  val stringToSquare: Map[String, Square] = squareStrings.map { case (str, sqr) => (sqr, str) }

}
