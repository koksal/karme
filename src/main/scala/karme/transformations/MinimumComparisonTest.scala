package karme.transformations

class MinimumComparisonTest extends DistributionComparisonTest {

  def testPValue(greaterSeq: Seq[Double], lessSeq: Seq[Double]): Double = {
    if (greaterSeq.min > lessSeq.min) {
      0.0
    } else {
      1.0
    }
  }

}
