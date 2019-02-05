package chess.model

trait Model[T <: Types] {

  val t: T

  type Pos <: Position[T]
  type Mov <: Move[T]
  type Not <: Notation[T, Pos, Mov]
  type Gam <: Game[T, Pos, Mov]
  type Rul <: Rules[T, Pos, Mov, Gam]

}
