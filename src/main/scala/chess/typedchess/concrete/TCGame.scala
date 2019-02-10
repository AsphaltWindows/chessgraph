package chess.typedchess.concrete

import chess.model.Game

import scala.collection.mutable.{MutableList => MutList}
import scala.collection.mutable.{Map => MutMap}

class TCGame extends Game[TCTypes.type, TCPosition, TCMove] {
  override val types: TCTypes.type = TCTypes

  override def moveHistory: Seq[TCMove] = Seq() ++ moveHist

  override def positionHistory: Seq[TCPosition] = Seq() ++ positionHist

  override def positionFrequency(position: TCPosition): Int = positionFreq.getOrElse(position.id, 0)

  override def fiftyMoveCount: Int = fiftyMoveRuleCounter

  override def advanceGame(move: TCMove, position: TCPosition): Unit = {
    moveHist += move
    positionHist += currentPosition
    val posId = position.id
    val oldFreq = positionFreq.getOrElse(oldFreq, 0)
    positionFreq.update(posId, oldFreq + 1)
    currentPosition = position
  }

  protected val moveHist: MutList[TCMove] = MutList()

  protected val positionHist: MutList[TCPosition] = MutList()

  protected val positionFreq: MutMap[String, Int] = MutMap()

  var currentPosition: TCPosition = new TCPosition()

  var fiftyMoveRuleCounter: Int = 0

}


