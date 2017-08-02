package karme.transformations

trait DistributionComparisonTest {

  def testPValue(greaterSeq: Seq[Double], lessSeq: Seq[Double]): Double

}

object DistributionComparisonTest {

  def fromOptions(name: String) = name match {
    case "ranksum" => new RankSumTest
    case "ks" => new KolmogorovSmirnovTest
    case _ => sys.error("Unknown distribution comparison method.")
  }

}
