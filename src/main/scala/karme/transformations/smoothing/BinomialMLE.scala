package karme.transformations.smoothing

import karme.CellTrajectories
import karme.CellTrajectories.CellTrajectory
import karme.Experiments.{BooleanExperiment, Experiment, Measurement, ProbabilisticExperiment}
import karme.synthesis.Transitions.GenericState

object BinomialMLE {

  def run(
    exp: BooleanExperiment,
    trajectories: Seq[CellTrajectory],
    radius: Int
  ): ProbabilisticExperiment = {
    // order each curve
    val cellOrders = trajectories map CellTrajectories.cellOrder

    // for each cell, get all cells within radius (vicinity)
    val probMeasurements = for (measurement <- exp.measurements) yield {
      val vicinityCellIDs = cellIDsInVicinity(measurement.id, cellOrders,
        radius)
      val vicinityMeasurements = vicinityCellIDs map { id =>
        exp.measurementFromId(id)
      }
      val expInVicinity = Experiment(vicinityMeasurements)

      // MLE is the proportion of 1 values across cells
      val mleValues = for (name <- exp.names) yield {
        val values = expInVicinity.valuesForName(name)
        name ->
          values.count(v => v).toDouble / values.size
      }

      Measurement(measurement.id, GenericState(mleValues.toMap))
    }

    Experiment(probMeasurements)
  }

  // may return duplicate cells if the cell is on multiple curves
  private def cellIDsInVicinity(
    cellId: String,
    cellOrders: Iterable[Seq[String]],
    radius: Int
  ): Seq[String] = {
    var result = Seq[String]()

    for (order <- cellOrders) {
      val i = order.indexOf(cellId)
      if (i >= 0) {
        // cell is in this curve, get all cells within radius
        val minI = math.max(0, i - radius)
        val maxI = math.min(order.size - 1, i + radius)

        // upper bound is exclusive
        result = result ++ order.slice(minI, maxI + 1)
      }
    }

    result
  }

}
