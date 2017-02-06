package karme.transformations

import karme.Experiments.BooleanExperiment
import karme.Experiments.ContinuousExperiment
import karme.Experiments.DiscreteExperiment
import karme.discretization.Discretization

object ExperimentTransformation {

  val ARCSINH_FACTOR = 2.0

  def arcsinh(v: Double, factor: Double): Double = {
    // arcsinh formula
    val scaled = v / factor
    val interior = scaled + math.sqrt(scaled * scaled + 1)
    math.log(interior)
  }

  def pseudoLog(
    experiment: ContinuousExperiment
  ): ContinuousExperiment = {
    val transformedMeasurements = experiment.measurements map { m =>
      m.copy(state = m.state.mapValues(pseudoLog(_, ARCSINH_FACTOR)))
    }
    experiment.copy(measurements = transformedMeasurements)
  }

  def pseudoLog(v: Double, factor: Double): Double = {
    arcsinh(v, factor) / math.log(10.0)
  }

  def namesWithOneLevel(exp: BooleanExperiment): Set[String] = {
    val nameSeq = exp.names filter { n =>
      val nvs = exp.valuesForName(n)
      // uses implicit Boolean ordering
      nvs.min == nvs.max
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
