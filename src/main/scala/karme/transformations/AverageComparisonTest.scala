package karme.transformations

import karme.util.MathUtil

class AverageComparisonTest extends DistributionComparisonTest {

  def testPValue(greaterSeq: Seq[Double], lessSeq: Seq[Double]): Double = {
    if (MathUtil.mean(greaterSeq) > MathUtil.mean(lessSeq)) {
      0.0
    } else {
      1.0
    }
  }

}
