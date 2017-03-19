package karme.transformations

import karme.Experiments.BooleanExperiment
import karme.Experiments.ContinuousExperiment

object ExperimentTransformation {

  def arcsinh(v: Double, factor: Double): Double = {
    // arcsinh formula
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

  def pseudoLog(v: Double, factor: Double): Double = {
    arcsinh(v, factor) / math.log(10.0)
  }

  def namesWithSingleValue(exp: BooleanExperiment): Set[String] = {
    val nameSeq = exp.names filter { n =>
      val nvs = exp.valuesForName(n)
      nvs.toSet.size == 1
    }
    nameSeq.toSet
  }

  def inactiveVariables(
    exp: BooleanExperiment, minCellActivityRatio: Double
  ): Set[String] = {
    val inactiveSeq = exp.names filter { name =>
      val vs = exp.valuesForName(name)
      val nbHigh = vs.count(v => v)
      val highRatio = nbHigh.toDouble / vs.size
      highRatio < minCellActivityRatio
    }
    inactiveSeq.toSet
  }
}
