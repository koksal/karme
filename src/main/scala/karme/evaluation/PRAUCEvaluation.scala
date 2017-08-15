package karme.evaluation

import java.io.File

import karme.Reporter
import karme.evaluation.enrichr.PredictionLibrary
import karme.util.FileUtil

class PRAUCEvaluation(reporter: Reporter) {

  val NB_RAND_TRIALS = 200

  def evaluate(
    predictions: Seq[((String, String), Int)],
    referenceEdges: Set[(String, String)],
    backgroundSources: Set[String],
    backgroundTargets: Set[String],
    referenceID: String
  ): Unit = {
    println(s"PR AUC evaluation for $referenceID")

    // compute original
    val auc = computeAucPR(predictions, referenceEdges,
      Some(reporter.file(s"pr-curve-$referenceID.pdf")))

    var betterThanRandomCount = 0

    for (i <- 1 to NB_RAND_TRIALS) {
      val randomizedPredictions =
        PairEvaluator.randomPredictionsWithUniqueScore(predictions.size,
          backgroundSources, backgroundTargets)

      val plotFile = if (i == 1) {
        Some(reporter.file(s"pr-curve-random-example-$referenceID.pdf"))
      } else {
        None
      }
      val randomAuc = computeAucPR(randomizedPredictions, referenceEdges,
        plotFile)

      if (auc > randomAuc) {
        betterThanRandomCount += 1
      }

      println(s"PR AUC trial $i, (${betterThanRandomCount.toDouble / i})")
    }

    FileUtil.writeToFile(reporter.file(s"auc-pr-$referenceID.txt"),
      s"AUC PR: Better than random in: " +
        s"$betterThanRandomCount / $NB_RAND_TRIALS")
  }

  def computeAucPR(
    predictions: Seq[((String, String), Int)],
    referencePairs: Set[(String, String)],
    curveFile: Option[File]
  ): Double = {
    val (truePositives, falsePositives) = predictions partition {
      case ((src, tgt), _) => referencePairs.contains((src, tgt))
    }

    val truePosScores = truePositives.map(_._2)
    val falsePosScores = falsePositives.map(_._2)

    val predictedPairs = predictions.map(_._1).toSet
    assert(predictedPairs.size == predictions.size)

    val falseNegatives = referencePairs -- predictedPairs
    val falseNegScores = falseNegatives.toSeq.map(_ => 0)

    val positiveScores = truePosScores ++ falseNegScores
    val negativeScores = falsePosScores

    new PRAUC().run(positiveScores, negativeScores, curveFile)
  }

}
