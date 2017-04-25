package karme.evaluation

import java.io.File

import com.github.tototoshi.csv.CSVReader
import karme.ArgHandling
import karme.evaluation.enrichr.{EnrichrPrediction, EnrichrPredictionLibrary}
import karme.util.FileUtil
import karme.util.MathUtil
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
    val orderedPredictions = predictionPairsByDescendingScore(predictedPairs)
    val orderedRefPairs = libraryPairsByDescendingScore(library)

    // get names from both sets. filter accordingly. then threshold.
    val namesInPredictions = namesInPairs(orderedPredictions)
    val namesInReference = namesInPairs(orderedRefPairs)

    val commonNames = namesInPredictions intersect namesInReference
    val backgroundSources = commonNames
    val backgroundTargets = commonNames

    val filteredPredictions = filterForNameUniverse(orderedPredictions,
      backgroundSources, backgroundTargets)
    val filteredRefPairs = filterForNameUniverse(orderedRefPairs,
      backgroundSources, backgroundTargets)

    println(s"Evaluating ${library.id}")

    val scoreMatrix = computeScoreMatrix(filteredPredictions,
      filteredRefPairs, backgroundSources, backgroundTargets)

    saveScoreMatrix(scoreMatrix, new File(s"score-matrix-${library.id}.csv"))
    saveScoreHeatmap(scoreMatrix, new File(s"heatmap-${library.id}.pdf"))

    countOrientationsPerThreshold(filteredPredictions)
  }

  private def thresholdRange: Seq[Double] = {
    val NB_THRESHOLD_STEPS = 10
    (1 to NB_THRESHOLD_STEPS) map (_ / NB_THRESHOLD_STEPS.toDouble)
  }

  private def computeScoreMatrix(
    predictedPairs: Seq[(String, String)],
    referencePairs: Seq[(String, String)],
    backgroundSources: Set[String],
    backgroundTargets: Set[String]
  ): Seq[Seq[Double]] = {
    for (predictionThreshold <- thresholdRange) yield {
      for (libraryPredictionThreshold <- thresholdRange) yield {
        val score = PredictionSignificanceTest.computeSignificance(
          filterByThreshold(predictedPairs, predictionThreshold).toSet,
          filterByThreshold(referencePairs, libraryPredictionThreshold).toSet,
          backgroundSources,
          backgroundTargets
        )

        println(s"pred: ${predictionThreshold}, ref: " +
          s"${libraryPredictionThreshold}, score: $score")
        score
      }
    }
  }

  private def saveScoreMatrix(matrix: Seq[Seq[Double]], f: File): Unit = {
    val content = matrix.map(_.mkString(",")).mkString("\n")

    FileUtil.writeToFile(f, content)
  }

  private def saveScoreHeatmap(matrix: Seq[Seq[Double]], f: File): Unit = {
    val labels = thresholdRange.map(_.toString)
    new Heatmap(matrix, "DB", "Predictions", labels, labels, f).run()
  }

  private def countOrientationsPerThreshold(
    predictedPairs: Seq[(String, String)]
  ): Unit = {
    for (threshold <- thresholdRange) {
      val meanCard = countOrientations(filterByThreshold(
        predictedPairs, threshold))
      println(s"Threshold: $threshold, avg. cardinality: $meanCard")
    }
  }

  private def countOrientations(pairs: Seq[(String, String)]): Double = {
    val groupedByValueSet = pairs.groupBy {
      case (src, tgt) => Set(src, tgt)
    }
    val cardinalities = groupedByValueSet.toSeq.map {
      case (nodeSet, pairs) =>
        assert(pairs.size == pairs.toSet.size)
        pairs.size.toDouble
    }
    MathUtil.mean(cardinalities)
  }

  private def filterForNameUniverse(
    pairs: Seq[(String, String)],
    possibleSources: Set[String],
    possibleTargets: Set[String]
  ): Seq[(String, String)] = {
    pairs filter {
      case (src, tgt) =>
        possibleSources.contains(src) && possibleTargets.contains(tgt)
    }
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

  def namesInPairs(pairs: Iterable[(String, String)]): Set[String] = {
    pairs.toSet[(String, String)] flatMap {
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
