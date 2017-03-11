package karme.discretization

import karme.Experiments.BooleanExperiment
import karme.Experiments.ContinuousExperiment
import karme.Experiments.Experiment
import karme.Experiments.Measurement
import karme.synthesis.Transitions.GenericState

sealed trait DiscretizationMethod
case object Ckmeans extends DiscretizationMethod
case object Mclust extends DiscretizationMethod

object Discretization {

  def binarize(
    experiment: ContinuousExperiment,
    method: DiscretizationMethod
  ): BooleanExperiment = {
    // transpose values to get values per name
    val valuesPerName = experiment.names.map(experiment.valuesForName(_))

    // discretize all values per name
    println("Running initial discretization.")
    val discreteCellValues =
      valuesPerName.map(vs => binarizeSeq(vs, method)).transpose

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

  private def binarizeSeq(
    vs: Seq[Double], method: DiscretizationMethod
  ): Seq[Boolean] = {
    val intValues = method match {
      case Ckmeans => {
        CkmeansInterface.ckmeans(vs, 1, 2)
      }
      case Mclust => {
        MclustInterface.run(vs, 1, 2).classification
      }
    }
    assert(intValues.forall(i => i >= 1 && i <= 2))
    intValues map (i => i == 2)
  }
}
