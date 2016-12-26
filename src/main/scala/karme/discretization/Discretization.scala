package karme.discretization

import karme.Experiments.ContinuousExperiment
import karme.Experiments.DiscreteExperiment
import karme.Experiments.Experiment
import karme.Experiments.Measurement
import karme.synthesis.Transitions.GenericState

object Discretization {

  val LOW_VALUE = 1
  val HIGH_VALUE = 2

  def discretize(experiment: ContinuousExperiment): DiscreteExperiment = {
    // transpose values to get values per name
    val valuesPerName = experiment.names.map(experiment.valuesForName(_))

    // discretize all values per name
    val discreteCellValues =
      valuesPerName.map(CkmeansInterface.ckmeans).transpose

    assert(discreteCellValues.size == experiment.measurements.size)

    val discreteMeasurements =
      experiment.measurements.map(_.id).zip(discreteCellValues) map {
      case (id, vs) => {
        val state = new GenericState(experiment.names.zip(vs).toMap)
        Measurement(id, state)
      }
    }

    Experiment(discreteMeasurements)
  }
}
