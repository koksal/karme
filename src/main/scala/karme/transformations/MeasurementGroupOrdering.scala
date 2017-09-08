package karme.transformations

import karme.Experiments.Measurement

class MeasurementGroupOrdering(
  measurementGroups: Seq[Seq[Measurement[Double]]],
  geneName: String,
  distributionComparisonTest: DistributionComparisonTest,
  pValueThreshold: Double
) extends Ordering[Int] with BinaryFunCache[Int, Double] {

  override def compare(x: Int, y: Int): Int = {
    val xGreater = get(x, y) <= pValueThreshold
    val yGreater = get(y, x) <= pValueThreshold

    assert(!(xGreater && yGreater))

    if (xGreater) {
      1
    } else if (yGreater) {
      -1
    } else {
      0
    }
  }

  override def compute(x: Int, y: Int): Double = {
    distributionComparisonTest.testPValue(
      geneValuesAtGroup(x),
      geneValuesAtGroup(y)
    )
  }

  private def geneValuesAtGroup(i: Int): Seq[Double] = {
    measurementGroups(i).map { measurement =>
      measurement.state.value(geneName)
    }
  }

}
