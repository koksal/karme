package karme

import karme.Experiments.Experiment

object CellTrajectories {

  type CellTrajectory = Map[String, Double]

  def cellOrder(trajectory: CellTrajectory): Seq[String] = {
    trajectory.toSeq.sortBy(_._2).map(_._1)
  }

  def orderMeasurementsByTrajectory[T](
    experiment: Experiment[T], trajectory: CellTrajectory
  ): Experiment[T] = {
    val orderedIDs = CellTrajectories.cellOrder(trajectory)
    val orderedMs = orderedIDs map experiment.measurementFromId
    experiment.copy(measurements = orderedMs)
  }
}
