package karme.transformations

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

  def pseudoLog(v: Double, factor: Double): Double = {
    arcsinh(v, factor) / math.log(10.0)
  }

  def pseudoLog(
    experiment: ContinuousExperiment
  ): ContinuousExperiment = {
    val transformedMeasurements = experiment.measurements map { m =>
      m.copy(state = m.state.mapValues(pseudoLog(_, ARCSINH_FACTOR)))
    }
    experiment.copy(measurements = transformedMeasurements)
  }

  def removeNamesWithOneLevel(exp: DiscreteExperiment): DiscreteExperiment = {
    val namesWithMultipleLevels = exp.names filter { n =>
      val nvs = exp.valuesForName(n)
      nvs.min < nvs.max
    }

    val nbRemovedDimensions = exp.names.size - namesWithMultipleLevels.size
    println(s"Removed ${nbRemovedDimensions} dimensions")
    exp.project(namesWithMultipleLevels.toSet)
  }

  def removeMostlyInactiveVariables(
    exp: DiscreteExperiment
  ): DiscreteExperiment = {
    val INACTIVE_CELL_RATIO_THRESHOLD = 0.80
    val activeVariables = exp.names filter { name =>
      val vs = exp.valuesForName(name)
      val nbHigh = vs.count(_ == Discretization.HIGH_VALUE)
      val nbLow = vs.count(_ == Discretization.LOW_VALUE)
      assert(nbHigh + nbLow == vs.size)
      val lowRatio = nbLow.toDouble / vs.size
      println(s"${name} low value ratio: ${lowRatio}")
      lowRatio < INACTIVE_CELL_RATIO_THRESHOLD
    }
    println(s"Reducing experiment to ${activeVariables.size} active variables.")
    exp.project(activeVariables.toSet)
  }
}
