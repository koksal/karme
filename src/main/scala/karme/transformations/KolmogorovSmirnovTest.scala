package karme.transformations

import karme.external.AbstractRInterface

class KolmogorovSmirnovTest extends AbstractRInterface
  with DistributionComparisonTest {

  def testPValue(greaterSeq: Seq[Double], lessSeq: Seq[Double]): Double = {
    R.set("xs", greaterSeq.toArray)
    R.set("ys", lessSeq.toArray)

    call("ks.test", "res",
      "x" -> "xs",
      "y" -> "ys",
      "alternative" -> "\"less\""
    )

    R.evalD0("res$p.value")
  }
}
