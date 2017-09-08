package karme.transformations

import karme.Experiments.Measurement
import karme.transformations.ExpressionDerivation.Downregulated
import karme.transformations.ExpressionDerivation.ExpressionDerivative
import karme.transformations.ExpressionDerivation.Unchanged
import karme.transformations.ExpressionDerivation.Upregulated

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

}
