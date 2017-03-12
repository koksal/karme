package karme.discretization

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

class CkmeansInterface(
  xs: Seq[Double], minNbClust: Int, maxNbClust: Int
) extends AbstractRInterface[Seq[Int]] {

  override val LIBRARIES: Seq[String] = Seq("Ckmeans.1d.dp")

  def process(R: RClient): Seq[Int] = {
    R.set("xs", xs.toArray)
    R.eval(s"k = c(${minNbClust}, ${maxNbClust})")
    R.eval(s"result = Ckmeans.1d.dp(xs, k = k)")
    R.evalI1("result$cluster")
  }
}
