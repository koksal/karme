package karme.transformations.discretization

import karme.external.AbstractRInterface

class CkmeansInterface extends AbstractRInterface {

  override def LIBRARIES: Seq[String] = Seq("Ckmeans.1d.dp")

  def cluster(
    xs: Seq[Double], minNbClust: Int, maxNbClust: Int
  ): Seq[Int] = {
    R.set("xs", xs.toArray)
    R.eval(s"k = c(${minNbClust}, ${maxNbClust})")
    R.eval(s"result = Ckmeans.1d.dp(xs, k = k)")
    R.evalI1("result$cluster")
  }
}
