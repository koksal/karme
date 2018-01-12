package karme.evaluation.synthetic

import karme.CellTrajectories.CellTrajectory

import scala.util.Random

class CellTrajectoryNoise(
  sigma: Double
) {

  val rand = new Random()

  def noise(): Double = {
    rand.nextGaussian() * sigma
  }

  def addNoise(t: CellTrajectory): CellTrajectory = {
    t map {
      case (k, v) => (k, v + noise())
    }
  }

}
