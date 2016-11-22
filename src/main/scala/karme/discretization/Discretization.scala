package karme.discretization

import karme.Experiments.ContinuousExperiment
import karme.Experiments.DiscreteExperiment
import karme.Experiments.Experiment
import karme.Experiments.Measurement

object Discretization {

  val LOW_VALUE = 1
  val HIGH_VALUE = 2

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
        Measurement(id, vs)
      }
    }

    Experiment(experiment.names, discreteMeasurements)
  }
}
