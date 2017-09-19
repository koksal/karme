package karme.transformations

import karme.Experiments.{ContinuousExperiment, Experiment}
import karme.util.MathUtil

object ExperimentTransformation {

  def arcsinh(v: Double, factor: Double): Double = {
    val scaled = v / factor
    val interior = scaled + math.sqrt(scaled * scaled + 1)
    math.log(interior)
  }

  def pseudoLog(
    experiment: ContinuousExperiment,
    factor: Double
  ): ContinuousExperiment = {
    val transformedMeasurements = experiment.measurements map { m =>
      m.copy(state = m.state.mapValues(pseudoLog(_, factor)))
    }
    experiment.copy(measurements = transformedMeasurements)
  }

  def scaleToUnitStdev(xs: Seq[Double]): Seq[Double] = {
    val sd = MathUtil.stdev(xs)
    val scaling = 1 / sd
    xs map (_ * scaling)
  }

  def scaleToUnitStdev(exp: Experiment[Double]): Experiment[Double] = {
    exp.transformValuesForEachName(scaleToUnitStdev)
  }

  def pseudoLog(v: Double, factor: Double): Double = {
    arcsinh(v, factor) / math.log(10.0)
  }

}
