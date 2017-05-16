package karme.evaluation

import java.io.File

import karme.Reporter
import karme.evaluation.enrichr.EnrichrPredictionLibrary
import karme.util.FileUtil
import karme.visualization.Heatmap

class HypergeometricEvaluation(reporter: Reporter) {

  val P_VALUE_THRESHOLD = 0.05

  val NB_DISCRIM_STEPS = 10

  def evaluate(
    predictedPairs: Seq[((String, String), Int)],
    library: EnrichrPredictionLibrary
  ): Unit = {
    val referencePairs = library.ioPairs

    val commonNames = getCommonNames(predictedPairs, referencePairs.toSeq)
    val backgroundSources = commonNames
    val backgroundTargets = commonNames

    val filteredPredictions = filterTriplesForNameUniverse(predictedPairs,
      backgroundSources, backgroundTargets)
    val filteredRefPairs = filterPairsForNameUniverse(referencePairs,
      backgroundSources, backgroundTargets)

    println(s"Evaluating ${library.id}")

    val scores = computeScoreByDiscrimination(filteredPredictions,
      filteredRefPairs, backgroundSources, backgroundTargets)

    println("Scores:")
    println(scores.mkString("\n"))
  }

  def getCommonNames(
    predictedPairs: Seq[((String, String), Int)],
    referencePairs: Seq[(String, String)]
  ): Set[String] = {
    // get names from both sets. filter accordingly. then threshold.
    val namesInPredictions = namesInPairs(predictedPairs.map(_._1))
    val namesInReference = namesInPairs(referencePairs)

    namesInPredictions intersect namesInReference
  }

  def namesInPairs(pairs: Iterable[(String, String)]): Set[String] = {
    pairs.toSet[(String, String)] flatMap {
      case (src, tgt) => Set(src, tgt)
    }
  }

  private def filterPairsForNameUniverse(
    pairs: Set[(String, String)],
    possibleSources: Set[String],
    possibleTargets: Set[String]
  ): Set[(String, String)] = {
    pairs filter {
      case (src, tgt) =>
        possibleSources.contains(src) && possibleTargets.contains(tgt)
    }
  }

  private def filterTriplesForNameUniverse(
    pairs: Seq[((String, String), Int)],
    possibleSources: Set[String],
    possibleTargets: Set[String]
  ): Seq[((String, String), Int)] = {
    pairs filter {
      case ((src, tgt), i) =>
        possibleSources.contains(src) && possibleTargets.contains(tgt)
    }
  }

  def predictionPairsByDescendingScore(
    predictedPairs: Seq[((String, String), Int)]
  ): Seq[(String, String)] = {
    val sortedPreds = predictedPairs.sortBy(_._2).reverse
    sortedPreds map {
      case ((src, tgt), _) => (src, tgt)
    }
  }

  def thresholdRange: Seq[Double] = {
    (1 to NB_DISCRIM_STEPS) map (_ / NB_DISCRIM_STEPS.toDouble)
  }

  private def computeScoreByDiscrimination(
    predictedPairs: Seq[((String, String), Int)],
    referencePairs: Set[(String, String)],
    backgroundSources: Set[String],
    backgroundTargets: Set[String]
  ): Seq[(Int, Double)] = {
    for (predictionThreshold <- thresholdRange) yield {
      val filteredPredictions = filterByThreshold(predictedPairs,
        predictionThreshold).toSet

      val score = PredictionSignificanceTest.computeSignificance(
        filteredPredictions,
        referencePairs,
        backgroundSources,
        backgroundTargets
      )

      println(s"Discr. threshold: ${predictionThreshold}, Nb predictions: " +
        s"${filteredPredictions.size}, score: $score")

      (filteredPredictions.size, score)
    }
  }

  private def binarizeScoreMatrix(
    matrix: Seq[Seq[Double]], threshold: Double
  ): Seq[Seq[Double]] = {
    def transform(e: Double): Double = {
      if (e < threshold) {
        0
      } else {
        1
      }
    }
    matrix.map(row => row.map(transform))
  }

  private def matrixHasSingleValue(matrix: Seq[Seq[Double]]): Boolean = {
    matrix.flatten.toSet.size == 1
  }

  private def saveScoreMatrix(matrix: Seq[Seq[Double]], f: File): Unit = {
    val content = matrix.map(_.mkString(",")).mkString("\n")

    FileUtil.writeToFile(f, content)
  }

  private def saveScoreHeatmap(matrix: Seq[Seq[Double]], f: File): Unit = {
    val labels = thresholdRange.map(_.toString)
    new Heatmap(matrix, "DB", "Predictions", labels, labels, f).run()
  }

  private def saveMinScore(matrix: Seq[Seq[Double]], f: File): Unit = {
    val minScore = matrix.map(_.min).min

    FileUtil.writeToFile(f, minScore.toString)
  }

  def filterByThreshold(
    pairs: Seq[((String, String), Int)], threshold: Double
  ): Seq[(String, String)] = {
    require(threshold >= 0 && threshold <= 1)

    val targetNbElems = (pairs.size * threshold).toInt

    val pairsUpToThreshold = pairs take targetNbElems
    val pairsAfterThreshold = pairs drop targetNbElems

    val lastScore = pairsUpToThreshold.last._2
    val pairsWithLastScore = pairsAfterThreshold.filter(_._2 == lastScore)

    (pairsUpToThreshold ++ pairsWithLastScore) map {
      case ((x, y), i) => (x, y)
    }
  }

}
