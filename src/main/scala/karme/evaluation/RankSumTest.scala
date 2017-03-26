package karme.evaluation

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

case class RankSumTestResult(statistic: Double, pValue: Double)

class RankSumTest(
  xs: Seq[Double], ys: Seq[Double]
) extends AbstractRInterface[RankSumTestResult] {

  def process(R: RClient): RankSumTestResult = {
    R.set("xs", xs.toArray)
    R.set("ys", ys.toArray)

    call(R)("wilcox.test", "res",
      "x" -> "xs",
      "y" -> "ys",
      "alternative" -> "\"greater\""
    )

    RankSumTestResult(R.evalD0("res$statistic"), R.evalD0("res$p.value"))
  }

}
