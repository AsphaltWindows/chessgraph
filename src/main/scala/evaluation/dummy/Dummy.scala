package evaluation.dummy

import chess.typedchess.concrete.TCPosition
import evaluation.model.Evaluation

object Dummy extends Evaluation {
  override type Input = TCPosition
  override type Result = Double
  override type Model = Int
  override type TrainingData = Int
  override type Cache = Int

  override def evaluate(model: Model, input: Input): Result = 0.5D

  override def newCache(): Cache = 0

  override def cacheEval(cache: Cache, input: Input, result: Result): Unit = {}

  override def deriveData(cache: Cache): Seq[TrainingData] = Seq()

  override def trainModel(model: Model, data: TrainingData): Unit = {}
}
