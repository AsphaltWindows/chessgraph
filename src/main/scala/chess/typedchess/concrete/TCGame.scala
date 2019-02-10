package chess.typedchess.concrete

import chess.model.Game
import chess.typedchess.concrete.TCGame._
import chess.typedchess.internal.state.InProgress

import scala.collection.mutable.{MutableList => MutList}
import scala.collection.mutable.{Map => MutMap}

class TCGame extends Game[TCTypes.type, TCPosition, TCMove] {
  override val types: TCTypes.type = TCTypes

  override def position: TCPosition = currentPosition

  override def moveHistory: Seq[TCMove] = Seq() ++ moveHist

  override def positionHistory: Seq[TCPosition] = Seq() ++ positionHist

  override def positionFrequency(position: TCPosition): Int = positionFreq.getOrElse(position.id, 0)

  override def fiftyMoveCount: Int = fiftyMoveRuleCounter

  override def gameState: types.State = state

  protected val moveHist: MutList[TCMove] = MutList()

  protected val positionHist: MutList[TCPosition] = MutList()

  protected val positionFreq: MutMap[String, Int] = MutMap()

  var currentPosition: TCPosition = new TCPosition()

  var fiftyMoveRuleCounter: Int = 0

  var state: types.State = InProgress

  positionFreq.update(currentPosition.id, 1)

  def handleOps(gameOps: Seq[GameOp]): Unit = {
    gameOps
      .foreach {
        case AdvanceGame(move, position) => {
          moveHist += move
          positionHist += currentPosition
          val posId = position.id
          val oldFreq = positionFreq.getOrElse(oldFreq, 0)
          positionFreq.update(posId, oldFreq + 1)
          currentPosition = position
        }
        case ResetFiftyMoveCounter =>
          fiftyMoveRuleCounter = 0
        case IncrementFiftyMoveCounter =>
          fiftyMoveRuleCounter += 1
        case SetState(newState) =>
          state = newState
      }
  }

}

object TCGame {

  import TCTypes._

  trait GameOp

  case object ResetFiftyMoveCounter extends GameOp

  case object IncrementFiftyMoveCounter extends GameOp

  case class AdvanceGame(move: TCMove, position: TCPosition) extends GameOp

  case class SetState(state: State) extends GameOp

}


