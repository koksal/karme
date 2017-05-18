package karme.evaluation

import java.io.File

import karme.Reporter
import karme.evaluation.enrichr.EnrichrPredictionLibrary
import karme.util.FileUtil

class PRAUCEvaluation(reporter: Reporter) {

  val NB_RAND_TRIALS = 100

  def evaluate(
    predictions: Seq[((String, String), Int)],
    library: EnrichrPredictionLibrary
  ): Unit = {
    println(s"Evaluating ${library.id}")

    // compute original
    val auc = computeAucPR(predictions, library,
      Some(reporter.file(s"pr-curve-${library.id}.pdf")))

    var betterThanRandomCount = 0

    for (i <- 1 to NB_RAND_TRIALS) {
      val randomizedPredictions =
        IOPairEvaluation.randomPredictionsWithSameScore(predictions)

      val randomAuc = computeAucPR(randomizedPredictions, library, None)

      if (auc > randomAuc) {
        betterThanRandomCount += 1
      }
    }

    FileUtil.writeToFile(reporter.file(s"auc-pr-${library.id}.txt"),
      s"AUC PR: Better than random in: " +
        s"$betterThanRandomCount / $NB_RAND_TRIALS")
  }

  def computeAucPR(
    predictions: Seq[((String, String), Int)],
    library: EnrichrPredictionLibrary,
    curveFile: Option[File]
  ): Double = {
    val referencePairs = library.ioPairs

    val (truePositives, falsePositives) = predictions partition {
      case ((src, tgt), score) => {
        referencePairs.contains((src, tgt))
      }
    }

    val truePosScores = truePositives.map(_._2)
    val falsePosScores = falsePositives.map(_._2)

    val predictedPairs = predictions.map(_._1).toSet
    assert(predictedPairs.size == predictions.size)

    val falseNegatives = referencePairs -- predictedPairs
    val falseNegScores = falseNegatives.toSeq.map(_ => 0)

    val positiveScores = truePosScores ++ falseNegScores
    val negativeScores = falsePosScores

    new PRAUC(positiveScores, negativeScores, curveFile).run()
  }

}
