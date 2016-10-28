package karme.analysis

import karme.CellTrajectories
import karme.CellTrajectories.CellTrajectory
import karme.Experiments.DiscreteExperiment
import karme.Experiments.Experiment
import karme.Experiments.Measurement
import karme.Experiments.ProbabilisticExperiment

object BinomialMLE {

  private val WINDOW_RADIUS = 20

  def run(
    exp: DiscreteExperiment,
    trajectories: Seq[CellTrajectory]
  ): ProbabilisticExperiment = {
    // order each curve
    val cellOrders = trajectories map CellTrajectories.cellOrder

    // for each cell
    //   get all vicinities (cells within radius)
    val probMeasurements = for (measurement <- exp.measurements) yield {
      val vicinityCellIDs = cellIDsInVicinity(measurement.id, cellOrders)
      val vicinityMeasurements = vicinityCellIDs map { id =>
        exp.measurementFromId(id)
      }
      val expInVicinity = Experiment(exp.names, vicinityMeasurements)

      // MLE is the proportion of 1 values across cells
      val mleValues = for (name <- exp.names) yield {
        val values = expInVicinity.valuesForName(name)
        values.count(_ == 1).toDouble / values.size
      }

      Measurement(measurement.id, mleValues)
    }

    Experiment(exp.names, probMeasurements)
  }

  // may return duplicate cells if the cell is on multiple curves
  private def cellIDsInVicinity(
    cellId: String,
    cellOrders: Iterable[Seq[String]]
  ): Seq[String] = {
    var result = Seq[String]()

    for (order <- cellOrders) {
      val i = order.indexOf(cellId)
      if (i >= 0) {
        // cell is in this curve, get all cells within radius
        val minI = math.max(0, i - WINDOW_RADIUS)
        val maxI = math.min(order.size - 1, i + WINDOW_RADIUS)

        // upper bound is exclusive
        result = result ++ order.slice(minI, maxI + 1)
      }
    }

    result
  }

}
