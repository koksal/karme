package karme.evaluation

import karme.external.AbstractRInterface

class HypergeometricTest extends AbstractRInterface {

  def test(
    nbSamples: Int,
    nbSampleSuccesses: Int,
    nbTotalSuccesses: Int,
    nbTotalFailures: Int
  ): Double = {
    call("phyper", "res",
      "q" -> (nbSampleSuccesses - 1),
      "m" -> nbTotalSuccesses,
      "n" -> nbTotalFailures,
      "k" -> nbSamples,
      "lower.tail" -> "FALSE")

    R.evalD0("res")
  }

}
