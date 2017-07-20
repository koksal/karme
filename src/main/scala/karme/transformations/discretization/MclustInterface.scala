package karme.transformations.discretization

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

case class MClustResult(
  g: Int, classification: Seq[Int], uncertainty: Seq[Double]
)

class MclustInterface extends AbstractRInterface {

  override def LIBRARIES = Seq("mclust")

  def cluster(
    xs: Seq[Double], minNbClust: Int, maxNbClust: Int
  ): MClustResult = {
    R.set("xs", xs.toArray)
    R.eval(s"res = Mclust(xs, G = c(${minNbClust}, ${maxNbClust}))")

    val optimalNbComponents = R.evalI0("res$G")
    val classification = R.evalI1("res$classification")
    val uncertainty = R.evalD1("res$uncertainty")

    MClustResult(optimalNbComponents, classification, uncertainty)
  }

}
