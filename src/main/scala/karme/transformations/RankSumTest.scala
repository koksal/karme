package karme.transformations

import karme.external.AbstractRInterface

case class RankSumTestResult(statistic: Double, pValue: Double)

class RankSumTest extends AbstractRInterface {

  def test(greaterSeq: Seq[Double], lessSeq: Seq[Double]): RankSumTestResult = {
    R.set("xs", greaterSeq.toArray)
    R.set("ys", lessSeq.toArray)

    call("wilcox.test", "res",
      "x" -> "xs",
      "y" -> "ys",
      "alternative" -> "\"greater\""
    )

    RankSumTestResult(R.evalD0("res$statistic"), R.evalD0("res$p.value"))
  }

}
