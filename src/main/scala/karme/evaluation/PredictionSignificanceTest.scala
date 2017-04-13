package karme.evaluation

object PredictionSignificanceTest {

  def computeSignificance(
    predictedPairs: Set[(String, String)],
    referencePairs: Set[(String, String)],
    backgroundSources: Set[String],
    backgroundTargets: Set[String]
  ): Double = {
    computeSignificance(
      filterForNameUniverse(predictedPairs, backgroundSources,
        backgroundTargets),
      filterForNameUniverse(referencePairs, backgroundSources,
        backgroundTargets),
      backgroundSources.size * backgroundTargets.size
    )
  }

  private def filterForNameUniverse(
    pairs: Set[(String, String)],
    possibleSources: Set[String],
    possibleTargets: Set[String]
  ): Set[(String, String)] = {
    pairs filter {
      case (src, tgt) =>
        possibleSources.contains(src) && possibleTargets.contains(tgt)
    }
  }

  private def computeSignificance(
    predictedPairs: Set[(String, String)],
    referencePairs: Set[(String, String)],
    totalNbElements: Int
  ): Double = {
    // nb samples: prediction size
    val nbSamples = predictedPairs.size
    println(s"Nb samples: $nbSamples")

    // nb sample successes: intersection of pairs
    val nbSampleSuccesses = predictedPairs.intersect(referencePairs).size
    println(s"Nb sample successes: $nbSampleSuccesses")

    // total successes: reference size
    val nbTotalSuccesses = referencePairs.size
    println(s"Nb total successes: $nbTotalSuccesses")

    // total failures: all possible pairs - reference
    val nbTotalFailures = totalNbElements - nbTotalSuccesses
    println(s"Nb total failures: $nbTotalFailures")

    new HypergeometricTest(
      nbSamples = nbSamples,
      nbSampleSuccesses = nbSampleSuccesses,
      nbTotalSuccesses = nbTotalSuccesses,
      nbTotalFailures = nbTotalFailures
    ).run()
  }

}
