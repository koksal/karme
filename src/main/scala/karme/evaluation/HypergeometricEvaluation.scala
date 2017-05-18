package karme.evaluation

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import karme.Reporter
import karme.evaluation.PredictionSignificanceTest.SignificanceResult
import karme.evaluation.enrichr.EnrichrPredictionLibrary
import karme.util.FileUtil
import karme.visualization.ScatterPlot

case class HypergeomEvalResult(
  nbPredictions: Int,
  hgPValue: Double,
  foldEnrichment: Double,
  avgCardinality: Double
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
    println(s"Background universe set size: ${backgroundUniv.size}")

    val filteredPredictions = filterTriplesForNameUniverse(predictedPairs,
      backgroundSources, backgroundTargets)
    val filteredRefPairs = filterPairsForNameUniverse(referencePairs,
      backgroundSources, backgroundTargets)

    println(s"Evaluating ${library.id}")

    val scores = computeScoreByDiscrimination(filteredPredictions,
      filteredRefPairs, backgroundSources, backgroundTargets)

    saveScores(scores, reporter.file(s"significance-eval-${library.id}.csv"))
    plotScores(scores, library.id)

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
    println(s"Prediction universe size: ${namesInPredictions.size}")
    val namesInReference = IOPairEvaluation.namesInPairs(referencePairs)
    println(s"Ref universe size: ${namesInReference.size}")

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

      val SignificanceResult(hgPValue, foldEnrichment) =
        PredictionSignificanceTest.computeSignificance(filteredPredictions,
          referencePairs, backgroundSources, backgroundTargets)

      val avgCard = IOPairEvaluation.meanOrientationCardinality(
        filteredPredictions.toSeq)

      println(s"Discr. threshold: $predictionThreshold, Nb predictions: " +
        s"${filteredPredictions.size}, hgPValue: $hgPValue")

      HypergeomEvalResult(filteredPredictions.size, hgPValue, foldEnrichment,
        avgCard)
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

  def plotScores(results: Seq[HypergeomEvalResult], libraryId: String): Unit = {
    val pValuePoints = results map {
      case HypergeomEvalResult(n, p, fe, c) => (n, p, "HG p-value")
    }
    new ScatterPlot(pValuePoints,
      reporter.file(s"hg-p-values-$libraryId.pdf")).run()

    val foldEnrPoints = results map {
      case HypergeomEvalResult(n, p, fe, c) => (n, fe, "fold enrichment")
    }
    new ScatterPlot(foldEnrPoints,
      reporter.file(s"fold-enr-$libraryId.pdf")).run()

    val meanCardPoints = results map {
      case HypergeomEvalResult(n, p, fe, c) => (n, c, "Mean orientation")
    }
    new ScatterPlot(meanCardPoints,
      reporter.file(s"mean-cardinality-$libraryId.pdf")).run()
  }

  def saveScores(results: Seq[HypergeomEvalResult], f: File): Unit = {
    val writer = CSVWriter.open(f)

    val header = List("nb. predictions", "hg p-value", "fold enr.",
      "avg. cardinality")
    val tuples = results map {
      case HypergeomEvalResult(n, s, fe, c) => List(n, s, fe, c)
    }
    writer.writeAll(header +: tuples)

    writer.close()
  }
}
