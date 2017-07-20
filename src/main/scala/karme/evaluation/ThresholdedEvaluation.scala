package karme.evaluation

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import karme.Reporter
import karme.evaluation.Evaluation.ScoredPrediction
import karme.evaluation.PredictionSignificanceTest.SignificanceResult
import karme.util.FileUtil
import karme.visualization.ScatterPlot

case class ThresholdedEvalResult(
  nbPredictions: Int,
  hgPValue: Double,
  foldEnrichment: Double,
  recall: Double,
  avgCardinality: Double
)

class ThresholdedEvaluation(reporter: Reporter) {

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
  ): Seq[ThresholdedEvalResult] = {
    val predictionGroups =
      predictionSubsetsByUniqueThreshold(predictedPairs)
    for ((filteredPreds, minScore) <- predictionGroups) yield {
      val SignificanceResult(hgPValue, foldEnrichment) =
        PredictionSignificanceTest.computeSignificance(filteredPreds,
          referencePairs, backgroundSources, backgroundTargets)

      val avgCard = PairEvaluator.meanOrientationCardinality(
        filteredPreds.toSeq)

      val recall = computeRecall(filteredPreds, referencePairs)

      println(s"Minimum score: $minScore, Nb predictions: " +
        s"${filteredPreds.size}, hgPValue: $hgPValue")

      ThresholdedEvalResult(filteredPreds.size, hgPValue, foldEnrichment,
        recall, avgCard)
    }
  }

  def computeRecall(
    predictions: Set[(String, String)], references: Set[(String, String)]
  ): Double = {
    val common = predictions.intersect(references)
    common.size.toDouble / references.size.toDouble
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

  def plotScores(
    results: Seq[ThresholdedEvalResult], libraryId: String
  ): Unit = {
    val scatterPlotInterface = new ScatterPlot()

    val pValuePoints = results map { r =>
      (r.nbPredictions, r.hgPValue, "HG p-value")
    }
    scatterPlotInterface.plot(
      pValuePoints,
      reporter.file(s"linear-hg-p-values-$libraryId.pdf"),
      logYScale = false
    )
    scatterPlotInterface.plot(
      pValuePoints,
      reporter.file(s"log-hg-p-values-$libraryId.pdf"),
      logYScale = true
    )

    val foldEnrPoints = results map { r =>
      (r.nbPredictions, r.foldEnrichment, "fold enrichment")
    }
    scatterPlotInterface.plot(foldEnrPoints,
      reporter.file(s"fold-enr-$libraryId.pdf"))

    val recallPoints = results map { r =>
      (r.nbPredictions, r.recall, "Recall")
    }
    scatterPlotInterface.plot(recallPoints,
      reporter.file(s"recall-$libraryId.pdf"))

    val meanCardPoints = results map { r =>
      (r.nbPredictions, r.avgCardinality, "Mean orientation")
    }
    scatterPlotInterface.plot(meanCardPoints,
      reporter.file(s"mean-cardinality-$libraryId.pdf"))
  }

  def saveScores(results: Seq[ThresholdedEvalResult], f: File): Unit = {
    val writer = CSVWriter.open(f)

    val header = List("nb. predictions", "hg p-value", "fold enr.",
      "avg. cardinality")
    val tuples = results map { r =>
      List(r.nbPredictions, r.hgPValue, r.foldEnrichment, r.avgCardinality)
    }
    writer.writeAll(header +: tuples)

    writer.close()
  }
}
