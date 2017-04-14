package karme.evaluation

object PredictionSignificanceTest {

  def computeSignificance(
    predictedPairs: Set[(String, String)],
    referencePairs: Set[(String, String)],
    backgroundSources: Set[String],
    backgroundTargets: Set[String]
  ): Double = {
    require(predictedPairs.forall(p => checkBackground(p, backgroundSources,
      backgroundTargets)))
    require(referencePairs.forall(p => checkBackground(p, backgroundSources,
      backgroundTargets)))

    computeSignificance(
      predictedPairs,
      referencePairs,
      backgroundSources.size * backgroundTargets.size
    )
  }

  private def checkBackground(
    pair: (String, String),
    backgroundSources: Set[String],
    backgroundTargets: Set[String]
  ): Boolean = {
    backgroundSources.contains(pair._1) && backgroundTargets.contains(pair._2)
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
