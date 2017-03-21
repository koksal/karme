package karme.evaluation

object PredictionSignificanceTest {

  def computeSignificance(
    predictedPairs: Set[(String, String)],
    referencePairs: Set[(String, String)],
    nameUniverse: Set[String]
  ): Double = {
    // nb samples: prediction size
    val nbSamples = predictedPairs.size

    // nb sample successes: intersection of pairs
    val nbSampleSuccesses = predictedPairs.intersect(referencePairs).size

    // total successes: reference size
    val nbTotalSuccesses = referencePairs.size

    // total failures: all possible pairs - reference
    val nbTotalFailures = nbTotalPairs(nameUniverse) - nbTotalSuccesses

    new HypergeometricTest(
      nbSamples = nbSamples,
      nbSampleSuccesses = nbSampleSuccesses,
      nbTotalSuccesses = nbTotalSuccesses,
      nbTotalFailures = nbTotalFailures
    ).run()
  }

  def nbTotalPairs(nameUniverse: Set[String]): Int = {
    // do self edges count as possible pairs?
    ???
  }

}
