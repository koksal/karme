package karme.transformations

import karme.Experiments.Measurement
import karme.synthesis.Transitions.{ConcreteBooleanState, GenericState}

class PairwiseComparisonDiscretization(
  distributionComparisonTest: DistributionComparisonTest,
  pValueThreshold: Double
) {

  def makeBooleanStatesPerGroup(
    measurementGroups: Seq[Seq[Measurement[Double]]]
  ): Seq[ConcreteBooleanState] = {
    val names = measurementGroups.head.head.state.orderedKeys

    val booleanValuesPerName = names map (
      n => discretizeGene (measurementGroups, n))

    booleanValuesPerName.transpose map { valuesPerGroup =>
      GenericState(names.zip(valuesPerGroup).toMap)
    }
  }

  private def discretizeGene(
    measurementGroups: Seq[Seq[Measurement[Double]]],
    name: String
  ): Seq[Boolean] = {
    val ordering = new MeasurementGroupOrdering(measurementGroups, name,
      distributionComparisonTest, pValueThreshold)

    for (i <- 0 until measurementGroups.size) yield {
      val isMinimal = (0 until measurementGroups.size).forall { j =>
        !ordering.lt(j, i)
      }
      !isMinimal
    }
  }

}
