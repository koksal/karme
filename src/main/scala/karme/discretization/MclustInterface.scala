package karme.discretization

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

case class MClustResult(
  g: Int, classification: Seq[Int], uncertainty: Seq[Double]
)

object MclustInterface extends
  AbstractRInterface[(Seq[Double], Int, Int), MClustResult] {

  def process(R: RClient)(arg: (Seq[Double], Int, Int)): MClustResult = {
    (processAux(R) _).tupled(arg)
  }

  private def processAux(R: RClient)(
    xs: Seq[Double], minNbClust: Int, maxNbClust: Int
  ): MClustResult = {
    R.eval("library(mclust)")

    R.set("xs", xs.toArray)
    R.eval(s"bic = mclustBIC(xs, G = c(${minNbClust}, ${maxNbClust}))")
    R.eval("res = Mclust(xs, x = bic)")

    val optimalNbComponents = R.evalI0("res$G")
    val classification = R.evalI1("res$classification")
    val uncertainty = R.evalD1("res$uncertainty")

    MClustResult(optimalNbComponents, classification, uncertainty)
  }

}
