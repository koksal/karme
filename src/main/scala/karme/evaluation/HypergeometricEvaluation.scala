package karme.evaluation

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import karme.Reporter
import karme.evaluation.Evaluation.ScoredPrediction
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

  def evaluate(
    predictedPairs: Seq[ScoredPrediction],
    referenceEdges: Set[(String, String)],
    backgroundSources: Set[String],
    backgroundTargets: Set[String],
    referenceID: String
  ): Unit = {

    println(s"Evaluating $referenceID")

    val scores = computeScoreByDiscrimination(predictedPairs,
      referenceEdges, backgroundSources, backgroundTargets)

    saveScores(scores, reporter.file(s"significance-eval-$referenceID.csv"))
    plotScores(scores, referenceID)

    val libraryOrientationCard = PairEvaluator.meanOrientationCardinality(
      referenceEdges.toSeq)
    FileUtil.writeToFile(
      reporter.file(s"orientation-cardinality-$referenceID.csv"),
      libraryOrientationCard.toString)
  }

  def computeScoreByDiscrimination(
    predictedPairs: Seq[ScoredPrediction],
    referencePairs: Set[(String, String)],
    backgroundSources: Set[String],
    backgroundTargets: Set[String]
  ): Seq[HypergeomEvalResult] = {
    val predictionGroups = predictionSubsetsByUniqueThreshold(predictedPairs)
    for ((filteredPreds, minScore) <- predictionGroups) yield {
      val SignificanceResult(hgPValue, foldEnrichment) =
        PredictionSignificanceTest.computeSignificance(filteredPreds,
          referencePairs, backgroundSources, backgroundTargets)

      val avgCard = PairEvaluator.meanOrientationCardinality(
        filteredPreds.toSeq)

      println(s"Minimum score: $minScore, Nb predictions: " +
        s"${filteredPreds.size}, hgPValue: $hgPValue")

      HypergeomEvalResult(filteredPreds.size, hgPValue, foldEnrichment,
        avgCard)
    }
  }

  def predictionSubsetsByUniqueThreshold(
    preds: Seq[ScoredPrediction]
  ): Seq[(Set[(String, String)], Int)] = {
    val uniqDescendingScores = preds.map(_._2).distinct.sorted.reverse

    for (scoreThreshold <- uniqDescendingScores) yield {
      val pairs = preds collect {
        case ((src, tgt), score) if score >= scoreThreshold => {
          (src, tgt)
        }
      }
      (pairs.toSet, scoreThreshold)
    }
  }

  def plotScores(results: Seq[HypergeomEvalResult], libraryId: String): Unit = {
    val pValuePoints = results map {
      case HypergeomEvalResult(n, p, fe, c) => (n, p, "HG p-value")
    }
    new ScatterPlot(
      pValuePoints,
      reporter.file(s"linear-hg-p-values-$libraryId.pdf"),
      logYScale = false
    ).run()
    new ScatterPlot(
      pValuePoints,
      reporter.file(s"log-hg-p-values-$libraryId.pdf"),
      logYScale = true
    ).run()

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
