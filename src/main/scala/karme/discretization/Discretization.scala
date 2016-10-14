package karme.discretization

import karme.ContinuousExperiment
import karme.DiscreteCellMeasurement
import karme.DiscreteExperiment

object Discretization {
  def discretize(experiment: ContinuousExperiment): DiscreteExperiment = {
    // transpose values to get values per name
    val transposedValues = experiment.measurements.map(_.values).transpose

    // discretize all values per name
    val discreteCellValues =
      transposedValues.map(CkmeansInterface.ckmeans).transpose

    assert(discreteCellValues.size == experiment.measurements.size)

    val discreteMeasurements =
      experiment.measurements.map(_.id).zip(discreteCellValues) map {
      case (id, vs) => {
        DiscreteCellMeasurement(id, vs)
      }
    }

    DiscreteExperiment(experiment.names, discreteMeasurements)
  }

}
