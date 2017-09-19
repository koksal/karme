package karme.transformations

import karme.Experiments.Measurement
import karme.transformations.ExpressionDerivation.{Downregulated, ExpressionDerivative, Unchanged, Upregulated}

class HierarchicalDerivatives(
  distributionComparisonTest: DistributionComparisonTest,
  pValueThreshold: Double
) {

  def makeDerivativesPerGroup(
    measurementGroups: Seq[Seq[Measurement[Double]]]
  ): Map[String, Seq[ExpressionDerivative]] = {
    val n = measurementGroups.size
    val names = measurementGroups.head.head.state.orderedKeys

    var result = Map[String, Seq[ExpressionDerivative]]()

    for (name <- names) {
      val ordering = new MeasurementGroupOrdering(measurementGroups, name,
        distributionComparisonTest, pValueThreshold)

      val derivatives = for (i <- 0 until (n - 1)) yield {
        val compRes = ordering.compare(i, i + 1)
        if (compRes < 0) {
          Upregulated
        } else if (compRes == 0) {
          Unchanged
        } else {
          Downregulated
        }
      }
      result += name -> derivatives
    }

    result
  }

  def makePValuesPerGroup(
    measurementGroups: Seq[Seq[Measurement[Double]]]
  ): Map[String, Seq[Double]] = {
    val n = measurementGroups.size
    val names = measurementGroups.head.head.state.orderedKeys

    var result = Map[String, Seq[Double]]()

    for (name <- names) {
      val ordering = new MeasurementGroupOrdering(measurementGroups, name,
        distributionComparisonTest, pValueThreshold)

      val pValues = for (i <- 0 until (n - 1)) yield {
        val leftToRightPVal = ordering.get(i, i + 1)
        val rightToLeftPVal = ordering.get(i + 1, i)
        math.min(leftToRightPVal, rightToLeftPVal)
      }

      result += name -> pValues
    }

    result
  }

}
