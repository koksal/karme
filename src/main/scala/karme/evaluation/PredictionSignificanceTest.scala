package karme.evaluation

object PredictionSignificanceTest {

  def computeSignificanceForNameUniverse(
    predictedPairs: Set[(String, String)],
    referencePairs: Set[(String, String)],
    nameUniverse: Set[String]
  ): Double = {
    computeSignificance(
      filterForNameUniverse(predictedPairs, nameUniverse),
      filterForNameUniverse(referencePairs, nameUniverse),
      nameUniverse
    )
  }

  def filterForNameUniverse(
    pairs: Set[(String, String)],
    nameUniverse: Set[String]
  ): Set[(String, String)] = {
    pairs filter {
      case (p1, p2) => nameUniverse.contains(p1) && nameUniverse.contains(p2)
    }
  }

  def filterOutSelfPairs(
    pairs: Set[(String, String)]
  ): Set[(String, String)] = {
    pairs filter {
      case (p1, p2) => p1 != p2
    }
  }

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
    val nbTotalFailures = nbTotalPairs(nameUniverse) -
      nbTotalSuccesses

    new HypergeometricTest(
      nbSamples = nbSamples,
      nbSampleSuccesses = nbSampleSuccesses,
      nbTotalSuccesses = nbTotalSuccesses,
      nbTotalFailures = nbTotalFailures
    ).run()
  }

  def nbTotalPairs(nameUniverse: Set[String]): Int = {
    val n = nameUniverse.size
    n * n
  }

}
