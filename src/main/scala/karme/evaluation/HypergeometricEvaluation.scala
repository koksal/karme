package karme.evaluation

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import karme.Reporter
import karme.evaluation.enrichr.EnrichrPredictionLibrary
import karme.util.FileUtil
import karme.visualization.ScatterPlot

case class HypergeomEvalResult(
  nbPredictions: Int, score: Double, avgCardinality: Double
)

class HypergeometricEvaluation(reporter: Reporter) {

  val P_VALUE_THRESHOLD = 0.05

  val NB_DISCRIM_STEPS = 100

  def evaluate(
    predictedPairs: Seq[((String, String), Int)],
    library: EnrichrPredictionLibrary
  ): Unit = {
    val referencePairs = library.ioPairs

    val backgroundUniv = getCommonNames(predictedPairs, referencePairs.toSeq)
    val backgroundSources = backgroundUniv
    val backgroundTargets = backgroundUniv

    val filteredPredictions = filterTriplesForNameUniverse(predictedPairs,
      backgroundSources, backgroundTargets)
    val filteredRefPairs = filterPairsForNameUniverse(referencePairs,
      backgroundSources, backgroundTargets)

    println(s"Evaluating ${library.id}")

    val scores = computeScoreByDiscrimination(filteredPredictions,
      filteredRefPairs, backgroundSources, backgroundTargets)

    saveScores(scores, reporter.file(s"hypergeom-eval-${library.id}.csv"))
    plotScores(scores, reporter.file(s"plot-hypergeom-${library.id}.pdf"))

    val libraryOrientationCard =
      IOPairEvaluation.meanOrientationCardinality(filteredRefPairs.toSeq)
    FileUtil.writeToFile(
      reporter.file(s"orientation-cardinality-${library.id}.csv"),
      libraryOrientationCard.toString)
  }

  def getCommonNames(
    predictedPairs: Seq[((String, String), Int)],
    referencePairs: Seq[(String, String)]
  ): Set[String] = {
    val namesInPredictions = IOPairEvaluation.namesInPairs(
      predictedPairs.map(_._1))
    val namesInReference = IOPairEvaluation.namesInPairs(referencePairs)

    namesInPredictions intersect namesInReference
  }

  def getUnionOfNames(
    predictedPairs: Seq[((String, String), Int)],
    referencePairs: Seq[(String, String)]
  ): Set[String] = {
    val namesInPredictions = IOPairEvaluation.namesInPairs(
      predictedPairs.map(_._1))
    val namesInReference = IOPairEvaluation.namesInPairs(referencePairs)

    namesInPredictions union namesInReference
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
  ): Seq[HypergeomEvalResult] = {
    for (predictionThreshold <- thresholdRange) yield {
      val filteredPredictions = filterByThreshold(predictedPairs,
        predictionThreshold).toSet

      val score = PredictionSignificanceTest.computeSignificance(
        filteredPredictions,
        referencePairs,
        backgroundSources,
        backgroundTargets
      )

      val avgCard = IOPairEvaluation.meanOrientationCardinality(
        filteredPredictions.toSeq)
      println(s"Discr. threshold: $predictionThreshold, Nb predictions: " +
        s"${filteredPredictions.size}, score: $score")
      println(s"Mean orientation: $avgCard")

      HypergeomEvalResult(filteredPredictions.size, score, avgCard)
    }
  }

  private def computeOrientationCardinalities(
    predictedPairs: Seq[((String, String), Int)]
  ): Seq[(Int, Double)] = {
    for (predictionThreshold <- thresholdRange) yield {
      val filteredPredictions = filterByThreshold(predictedPairs,
        predictionThreshold).toSet

      (filteredPredictions.size,
        IOPairEvaluation.meanOrientationCardinality(filteredPredictions.toSeq))
    }
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

  def plotScores(results: Seq[HypergeomEvalResult], f: File): Unit = {
    val scorePoints = results map {
      case HypergeomEvalResult(n, s, c) => (n, s, "HG p-value")
    }
    val meanCardPoints = results map {
      case HypergeomEvalResult(n, s, c) => (n, c, "Mean orientation")
    }

    new ScatterPlot(scorePoints ++ meanCardPoints, f).run()
  }

  def saveScores(results: Seq[HypergeomEvalResult], f: File): Unit = {
    val writer = CSVWriter.open(f)

    val header = List("nb. predictions", "score", "avg. cardinality")
    val tuples = results map {
      case HypergeomEvalResult(n, s, c) => List(n, s, c)
    }
    writer.writeAll(header +: tuples)

    writer.close()
  }
}
