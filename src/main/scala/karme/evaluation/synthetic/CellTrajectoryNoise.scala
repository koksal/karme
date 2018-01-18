package karme.evaluation.synthetic

import karme.CellTrajectories.CellTrajectory

import scala.util.Random

class CellTrajectoryNoise(
  sigma: Double,
  random: Random
) {

  def noise(): Double = {
    random.nextGaussian() * sigma
  }

  def addNoise(t: CellTrajectory): CellTrajectory = {
    t map {
      case (k, v) => (k, v + noise())
    }
  }

}
