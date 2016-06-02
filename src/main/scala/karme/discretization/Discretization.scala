package karme.discretization

import karme.Experiment
import karme.DiscreteExperiment
import karme.DiscreteCellMeasurement

case class DiscretizationResult(nbLevels: Int, values: Seq[Int])

object Discretization {
  // send each protein to ckmeans
  // read back number of clusters and discretized data
  def discretizeExperiment(e: Experiment): DiscreteExperiment = {
  
    val allValues = e.measurements.map(_.values)
    val protVectors = allValues.transpose
    val discrRes = protVectors map discretizeProtein
    val discrLevels = discrRes map (_.nbLevels)
    val discrProtVectors = discrRes map (_.values)
    val discrValues = discrProtVectors.transpose

    assert(e.measurements.size == discrValues.size)
    val discrMeasurements = e.measurements.zip(discrValues).map { case (cm, discrVs) =>
      DiscreteCellMeasurement(cm.time, cm.actualTime, cm.pseudotime, discrVs)
    }
    DiscreteExperiment(e.measuredProteins, discrMeasurements, discrLevels)
  }

  def discretizeProtein(xs: Seq[Double]): DiscretizationResult = {
    karme.RInterface.ckmeans(xs)
  }
}
