package karme.evaluation.synthetic

import karme.synthesis.Transitions.ConcreteBooleanState

import scala.util.Random

class MeasurementNoise(random: Random)(errorProbability: Double) {

  private def addValueNoise(b: Boolean): Boolean = {
    if (random.nextDouble() < errorProbability) {
      !b
    } else {
      b
    }
  }

  def addNoise(s: ConcreteBooleanState): ConcreteBooleanState = {
    s.mapValues(addValueNoise)
  }

}
