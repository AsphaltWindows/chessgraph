package chess.indexed2chess.concrete

import chess.model.Game
import chess.indexed2chess.concrete.ICGame._
import chess.indexed2chess.internal.state.InProgress

import scala.collection.mutable.{MutableList => MutList}
import scala.collection.mutable.{Map => MutMap}

class ICGame extends Game[ICTypes.type, ICPosition, ICMove] {
  override val types: ICTypes.type = ICTypes

  override def position: ICPosition = currentPosition

  override def moveHistory: Seq[ICMove] = Seq() ++ moveHist

  override def positionHistory: Seq[ICPosition] = Seq() ++ positionHist

  override def positionFrequency(position: ICPosition): Int = positionFreq.getOrElse(position.id, 0)

  override def fiftyMoveCount: Int = fiftyMoveRuleCounter

  override def gameState: types.State = state

  protected val moveHist: MutList[ICMove] = MutList()

  protected val positionHist: MutList[ICPosition] = MutList()

  protected val positionFreq: MutMap[String, Int] = MutMap()

  var currentPosition: ICPosition = new ICPosition()

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
          val oldFreq = positionFreq.getOrElse(posId, 0)
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

object ICGame {

  import ICTypes._

  trait GameOp

  case object ResetFiftyMoveCounter extends GameOp

  case object IncrementFiftyMoveCounter extends GameOp

  case class AdvanceGame(move: ICMove, position: ICPosition) extends GameOp

  case class SetState(state: State) extends GameOp

}


