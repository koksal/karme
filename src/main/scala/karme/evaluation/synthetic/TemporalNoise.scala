package karme.evaluation.synthetic

import scala.util.Random

class TemporalNoise(random: Random)(sigma: Double) {

  private def noise(): Double = {
    random.nextGaussian() * sigma
  }

  def addNoise(baseTime: Double): Double = {
    baseTime + noise()
  }

}
