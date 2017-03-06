package karme.discretization

import org.ddahl.rscala.RClient

case class MClustResult(
  g: Int, classification: Seq[Int], uncertainty: Seq[Double]
)

object MclustInterface {

  def mclust(xs: Seq[Double]): MClustResult = {
    val R = RClient()
    R.eval("library(mclust)")

    R.set("xs", xs.toArray)
    R.eval("bic = mclustBIC(xs, G = c(1, 2))")
    R.eval("res = Mclust(xs, x = bic)")

    val optimalNbComponents = R.evalI0("res$G")
    val classification = R.evalI1("res$classification")
    val uncertainty = R.evalD1("res$uncertainty")

    MClustResult(optimalNbComponents, classification, uncertainty)
  }

}
