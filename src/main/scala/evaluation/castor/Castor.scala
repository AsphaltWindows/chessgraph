package evaluation.castor

import evaluation.model.Evaluation
import org.platanios.tensorflow.api.tensors.Tensor

import scala.collection.mutable.{MutableList => MutSeq}

class Castor(errorThreshold: Double) extends Evaluation {

  override type Input = Tensor[Byte]
  override type Result = Double
  override type Model = Unit
  type Error = Double
  override type TrainingData = (Input, Error)
  override type Cache = MutSeq[(Input, Result)]

  override def evaluate(model: Model, input: Input): Result = ???

  override def newCache(): Cache = MutSeq()

  override def cacheAppend(cache: Cache, input: Input, result: Result): Unit = { cache += ((input, result)) }

  override def deriveData(cache: Cache): Seq[TrainingData] = {
    cache
      .sliding(2)
      .map { two =>
        val before = two.head
        val after = two(1)
        (before._1, before._2 - after._2)
      }
      .filter { case (_, err) =>
        err.abs > errorThreshold
      }
      .toSeq

  }

  override def trainModel(model: Model, data: TrainingData): Unit = ???

  override def readFromFile(filename: String): Model = ???

  override def writeToFile(fileName: String, model: Model): Unit = ???
}
