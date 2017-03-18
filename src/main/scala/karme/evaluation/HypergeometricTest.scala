package karme.evaluation

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

class HypergeometricTest(
  nbSamples: Int,
  nbSampleSuccesses: Int,
  nbTotalSuccesses: Int,
  nbTotalFailures: Int
) extends AbstractRInterface[Double] {

  def process(R: RClient): Double = {
    call(R)("phyper", "res",
      "q" -> (nbSampleSuccesses - 1),
      "m" -> nbTotalSuccesses,
      "n" -> nbTotalFailures,
      "k" -> nbSamples,
      "lower.tail" -> "FALSE")

    R.evalD0("res")
  }

}
