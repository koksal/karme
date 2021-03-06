package karme.transformations

trait DistributionComparisonTest {

  def testPValue(greaterSeq: Seq[Double], lessSeq: Seq[Double]): Double

}

object DistributionComparisonTest {

  def fromOptions(name: String): DistributionComparisonTest = name match {
    case "ranksum" => new RankSumTest
    case "ks" => new KolmogorovSmirnovTest
    case "average" => new AverageComparisonTest
    case "minimum" => new MinimumComparisonTest
    case _ => sys.error("Unknown distribution comparison method.")
  }

}
