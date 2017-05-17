package karme.evaluation

import karme.Reporter
import karme.evaluation.enrichr.EnrichrPredictionLibrary

class PRAUCEvaluation(reporter: Reporter) {

  val NB_RAND_TRIALS = 50

  def evaluate(
    predictions: Seq[((String, String), Int)],
    library: EnrichrPredictionLibrary
  ): Unit = {
    println(s"Evaluating ${library.id}")

    // compute original
    val auc = computeAucPR(predictions, library)
    println(s"Original (${predictions.size}):")
    println(auc)

    var betterThanRandomCount = 0

    for (i <- 1 to NB_RAND_TRIALS) {
      val randomizedPairs = IOPairEvaluation.randomPairsWithoutReplacement(
        IOPairEvaluation.namesInPairs(predictions.map(_._1)),
        predictions.size).toSet

      val randomizedPredictions = predictions.zip(randomizedPairs) map {
        case (pred, pair) => (pair, pred._2)
      }

      val randomAuc = computeAucPR(randomizedPredictions, library)
      println(s"Random run $i (${randomizedPairs.size})")
      println(randomAuc)

      if (auc > randomAuc) {
        betterThanRandomCount += 1
      }
    }

    println(s"Better than random in: $betterThanRandomCount / $NB_RAND_TRIALS")
  }

  def computeAucPR(
    predictions: Seq[((String, String), Int)],
    library: EnrichrPredictionLibrary
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

    new PRAUC(positiveScores, negativeScores).run()
  }

}
