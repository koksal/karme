package karme.evaluation

import java.io.File

import com.github.tototoshi.csv.CSVReader
import karme.ArgHandling
import karme.evaluation.enrichr.{EnrichrPrediction, EnrichrPredictionLibrary}
import karme.visualization.Heatmap

import scala.util.Random

object BigramEvaluation {

  val NB_RUNS = 3

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    val evalCtx = EvaluationContext.fromOptions(opts.evalOpts)

    val predictions = parsePredictions(opts.evalOpts.bigramFile.get)

    for (library <- evalCtx.references) {
      evaluate(predictions, library)
    }
  }

  def parsePredictions(f: File): Seq[(String, String, Int)] = {
    val reader = CSVReader.open(f)
    reader.all() map {
      case List(src, tgt, score) => (src, tgt, score.toInt)
    }
  }

  def evaluate(
    predictedPairs: Seq[(String, String, Int)],
    library: EnrichrPredictionLibrary
  ): Unit = {
    val nbThresholdSteps = 10
    val thresholdRange =
      (1 to nbThresholdSteps) map (_ / nbThresholdSteps.toDouble)

    val orderedPreds = predictionPairsByDescendingScore(predictedPairs)
    val randomPreds = randomBigrams(namesInPairs(orderedPreds.toSet),
      orderedPreds.size)
    val orderedLibraryPreds = libraryPairsByDescendingScore(library)

    println(s"Evaluating ${library.id}")

    val scoreMatrix = for (predictionThreshold <- thresholdRange) yield {
      for (libraryPredictionThreshold <- thresholdRange) yield {
        val score = evaluateForCommonSourcesAndTargets(
          filterByThreshold(randomPreds, predictionThreshold).toSet,
          filterByThreshold(orderedLibraryPreds,
            libraryPredictionThreshold).toSet
        )

        println(s"Pred t: ${predictionThreshold}, library t: " +
          s"${libraryPredictionThreshold}, score: $score")
        score
      }
    }

    println(library.id)
    println(scoreMatrix.map(_.mkString(",")).mkString("\n"))

    val f = new File(s"${library.id}-heatmap.pdf")
    val labels = thresholdRange.map(_.toString)
    new Heatmap(scoreMatrix, "DB", "Predictions", labels, labels, f).run()
  }

  def predictionPairsByDescendingScore(
    predictedPairs: Seq[(String, String, Int)]
  ): Seq[(String, String)] = {
    val sortedPreds = predictedPairs.sortBy(_._3).reverse
    sortedPreds map {
      case (src, tgt, _) => (src, tgt)
    }
  }

  def libraryPairsByDescendingScore(
    library: EnrichrPredictionLibrary
  ): Seq[(String, String)] = {
    val sortedPreds = library.predictions.sortBy(_.combinedScore).reverse
    sortedPreds map {
      case EnrichrPrediction(term, target, score) => (term, target)
    }
  }

  def filterByThreshold(
    pairs: Seq[(String, String)], threshold: Double
  ): Seq[(String, String)] = {
    require(threshold >= 0 && threshold <= 1)
    val toTake = (pairs.size * threshold).toInt
    pairs take toTake
  }

  def evaluateForCommonSourcesAndTargets(
    predictedPairs: Set[(String, String)],
    libraryPairs: Set[(String, String)]
  ): Double = {
    val predictedNames = namesInPairs(predictedPairs)
    val libraryNames = namesInPairs(libraryPairs)

    val commonNames = predictedNames.intersect(libraryNames)

    PredictionSignificanceTest.computeSignificance(predictedPairs, libraryPairs,
      predictedNames, predictedNames)
  }

  def namesInPairs(pairs: Set[(String, String)]): Set[String] = {
    pairs flatMap {
      case (src, tgt) => Set(src, tgt)
    }
  }

  def shuffleBigrams(
    originalBigrams: Seq[(String, String)]
  ): Seq[(String, String)] = {
    val (sources, targets) = originalBigrams.unzip
    val allNames = sources ++ targets

    val random = new Random()

    val shuffled = random.shuffle(allNames)
    val newSources = shuffled.take(originalBigrams.size)
    val newTargets = shuffled.drop(originalBigrams.size)

    newSources zip newTargets
  }

  def randomBigrams(
    nameUniverse: Set[String], size: Int
  ): Seq[(String, String)] = {
    val random = new Random()

    val indexedNames = nameUniverse.toIndexedSeq
    val n = indexedNames.size

    val sources = (1 to size).map(i => indexedNames(random.nextInt(n)))
    val targets = (1 to size).map(i => indexedNames(random.nextInt(n)))

    sources zip targets
  }

}
