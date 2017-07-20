package karme.transformations.discretization

import karme.Experiments.BooleanExperiment
import karme.Experiments.ContinuousExperiment
import karme.Experiments.Experiment
import karme.Experiments.Measurement
import karme.synthesis.Transitions.GenericState

sealed trait DiscretizationMethod
case object Ckmeans extends DiscretizationMethod
case object Mclust extends DiscretizationMethod
case object Thresholding extends DiscretizationMethod

object Discretization {

  def binarize(
    experiment: ContinuousExperiment,
    method: DiscretizationMethod
  ): BooleanExperiment = {
    println("Running initial discretization.")
    val discreteValueMatrix = experiment.valueMatrix.map{
      values => binarizeSeq(values, method)
    }
    val discreteCellValues = discreteValueMatrix.transpose
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
      case Thresholding => {
        ThresholdDiscretization(vs)
      }
      case Ckmeans => {
        new CkmeansInterface().cluster(vs, 1, 2)
      }
      case Mclust => {
        new MclustInterface().cluster(vs, 1, 2).classification
      }
    }
    assert(intValues.forall(i => i >= 1 && i <= 2))
    intValues map (i => i == 2)
  }

}
