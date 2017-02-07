package karme.discretization

import karme.Experiments.BooleanExperiment
import karme.Experiments.ContinuousExperiment
import karme.Experiments.Experiment
import karme.Experiments.Measurement
import karme.synthesis.Transitions.GenericState

object Discretization {

  val LOW_VALUE = 1
  val HIGH_VALUE = 2

  def binarize(experiment: ContinuousExperiment): BooleanExperiment = {
    // transpose values to get values per name
    val valuesPerName = experiment.names.map(experiment.valuesForName(_))

    // discretize all values per name
    println("Running ckmeans.")
    val discreteCellValues =
      valuesPerName.map(CkmeansInterface.ckmeans).transpose
    println()

    assert(discreteCellValues.size == experiment.measurements.size)

    val discreteMeasurements =
      experiment.measurements.map(_.id).zip(discreteCellValues) map {
      case (id, vs) => {
        val booleanValues = vs map (_ == HIGH_VALUE)
        val state = new GenericState(experiment.names.zip(booleanValues).toMap)
        Measurement(id, state)
      }
    }

    Experiment(discreteMeasurements)
  }
}
