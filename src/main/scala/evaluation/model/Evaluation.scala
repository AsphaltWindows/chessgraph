package evaluation.model

trait Evaluation {

  type Input
  type Result
  type Model
  type TrainingData
  type Cache

  def evaluate(model: Model, input: Input): Result

  def newCache(): Cache

  def cacheEval(cache: Cache, input: Input, result: Result): Unit

  def deriveData(cache: Cache): Seq[TrainingData]

  def trainModel(model: Model, data: TrainingData): Unit

}
