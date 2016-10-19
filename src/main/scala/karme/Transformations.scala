package karme

import karme.Experiments.ContinuousExperiment

object Transformations {

  val ARCSINH_FACTOR = 2.0

  def arcsinh(v: Double, factor: Double): Double = {
    // arcsinh formula
    val scaled = v / factor
    val interior = scaled + math.sqrt(scaled * scaled + 1)
    math.log(interior)
  }

  def pseudoLog(v: Double, factor: Double): Double = {
    arcsinh(v, factor) / math.log(10.0)
  }

  def pseudoLog(
    experiment: ContinuousExperiment
  ): ContinuousExperiment = {
    val transformedMeasurements = experiment.measurements map { m =>
      m.copy(values = m.values.map(pseudoLog(_, ARCSINH_FACTOR)))
    }
    experiment.copy(measurements = transformedMeasurements)
  }
}
