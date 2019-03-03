package tools

import scala.collection.mutable.{Map => MutMap}
import scala.collection.mutable.{MutableList => MutList}
import scala.io.StdIn
import scala.util.Random

object NestedBayesTest extends App {

  /** we will try to train a 2 layer nested bayes to train the output to be the same as the input **/

  // 3 input types
  object Classification extends Enumeration {
    val A = Value("A")
    val B = Value("B")
    val C = Value("C")
  }

  case class FrequencyTable[InputType, OutputType](input: Set[InputType], output: Set[OutputType]) {

    val frequencyIO: MutMap[(InputType, OutputType), Int] = MutMap() ++ input
      .flatMap { in =>
        output.map { out =>
          (in, out)
        }
      }
      .toSeq
      .map { k =>
        k -> 1
      }
      .toMap

    val frequencyI: MutMap[InputType, Int] = MutMap() ++ input.map { k => k -> output.size }

    val frequencyO: MutMap[OutputType, Int] = MutMap() ++ output.map { k => k -> input.size }

    var frequencyT: Int = input.size * output.size

    var chiSquareCache: Double = calculateChiSquared

    def chiSquared: Double = chiSquareCache

    private def probabilityForP(in: InputType): Map[OutputType, Double] = output
      .map { out =>
        (out, frequencyIO(in, out).toDouble / frequencyO(out).toDouble)
      }
      .toMap

    private def addOccurenceP(in: InputType, result: OutputType): Unit = {
      frequencyT += 1
      frequencyIO(in, result) += 1
      frequencyI(in) += 1
      frequencyO(result) += 1
      chiSquareCache = calculateChiSquared
    }

    private def calculateChiSquared: Double = input.flatMap { in =>
      output.map { out =>
        (in, out)
      }
    }
      .map { case (in, out) =>
        val expected = frequencyI(in) / frequencyT.toDouble * frequencyO(out).toDouble
        val actual = frequencyIO(in, out)
        val delta = actual - expected
        delta / expected * delta / frequencyT.toDouble
      }
      .sum

    def probabilityFor(in: Any): Map[OutputType, Double] = in match {
      case i: InputType => probabilityForP(i)
      case _ => throw new Exception("Invalid input passed into FrequencyTable")
    }

    def addOccurence(in: Any, result: OutputType): Unit = in match {
      case i: InputType => addOccurenceP(i, result)
      case _ => throw new Exception("Invalid occurence passed into FrequencyTable")
    }
  }


  case class BayesNeuron[OutputType](output: Set[OutputType]) {

    val tableList: MutList[FrequencyTable[_, OutputType]] = MutList()

    def addInput[I](ins: Set[I]): Unit = {
      val newTable = FrequencyTable(ins, output)
      tableList += newTable
    }

    def train(inputs: Seq[Any], result: OutputType): Seq[Double] = {
      tableList
        .zip(inputs)
        .map { case (tab, input) =>
          val chi1 = tab.chiSquared
          tab.addOccurence(input, result)
          val chi2 = tab.chiSquared
          chi2 - chi1
        }
    }

    def prediction(inputs: Seq[Any]): Map[OutputType, Double] = {
      tableList
        .zip(inputs)
        .map { case (tab, input) =>
          tab.probabilityFor(input)
        }
        .foldLeft[Map[OutputType, Double]](output.map(k => k -> 1D).toMap) {
        case (acc, ps) => output
          .map { k =>
            k -> (acc(k) * ps(k))
          }
          .toMap
      }
    }

    def mostLikely(inputs: Seq[Any]): OutputType = {
      prediction(inputs)
        .toSeq
        .reduceLeft[(OutputType, Double)] { case (last, next) => if (last._2 < next._2) next else last }
        ._1
    }

    def randomByProbability(inputs: Seq[Any]): OutputType = {
      val probabilities = prediction(inputs)
        .toSeq

      val totalProbability = probabilities
        .foldLeft[Double](0D) { case (acc, next) => acc + next._2 }

      val normFactor = 1D / totalProbability

      val normProbs = probabilities
        .foldLeft[Seq[(OutputType, Double)]](Seq()) { case (acc, (o, p)) =>
        (o, acc.headOption.map {
          _._2
        }.getOrElse(0D) + p * normFactor) +: acc
      }
        .reverse

      val rand = Random.nextDouble()

      normProbs.dropWhile { case (_, p) => rand > p }.head._1
    }
  }

  def train(inputN: Seq[BayesNeuron[Boolean]], outputN: BayesNeuron[Classification.Value], from: Classification.Value, to: Classification.Value): Unit = {

    val inputOutputs = inputN
      .map { neur =>
        neur.randomByProbability(Seq(from))
      }

    val chiSDeltas = outputN
      .train(inputOutputs, to)

    inputN
      .zip(inputOutputs)
      .zip(chiSDeltas)
      .foreach { case ((neur, output), delta) =>
        neur.train(Seq(from), !(output ^ (delta > 0)))
      }
  }

  def predict(inputN: Seq[BayesNeuron[Boolean]], outputN: BayesNeuron[Classification.Value], in: Classification.Value): Classification.Value = {
    val inputOutputs = inputN
      .map { neur =>
        neur.randomByProbability(Seq(in))
      }

    outputN
      .randomByProbability(inputOutputs)
  }


  override def main(args: Array[String]): Unit = {

    val neuronOut = BayesNeuron(Classification.values)

    val inputNeurons: MutList[BayesNeuron[Boolean]] = MutList()

    import Classification._

    val trainingData = (1 to 15)
      .flatMap { _ =>
        Seq((A, A), (B, B), (C, C))
      }

    (1 to 3)
      .foreach { _ =>
        val inNeuron = BayesNeuron(Set(true, false))
        inNeuron.addInput(Classification.values)
        neuronOut.addInput(Set(true, false))

        inputNeurons += inNeuron

        println("training neuron")

        trainingData
          .foreach { case (in, out) =>
            println(s"training neuron input: $in, output: $out")
            train(inputNeurons, neuronOut, in, out)
          }

        (1 to 5)
          .foreach { _ =>
            val toClassifyStr = StdIn.readLine("provide input to attempt to classify: ")
            val toClassify = Classification.withName(toClassifyStr)
            val predicted = predict(inputNeurons, neuronOut, toClassify)
            println(s"Predicted $toClassify to be $predicted")
          }

        println("Input neurons:")
        inputNeurons
          .foreach { n =>
            n.tableList
              .foreach { tab =>
                println("frequency tables: " + tab.frequencyIO)
                println("chi-squared: " + tab.chiSquared)
              }
          }

        println("Output neurons:")
        neuronOut
          .tableList
          .foreach { tab =>
            println("frequency tables: " + tab.frequencyIO)
            println("chi-squared: " + tab.chiSquared)
          }
      }
  }


}
