package karme.clustering

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

class NbClustInterface(
  matrix: Seq[Seq[Double]],
  minNbClust: Int,
  maxNbClust: Int,
  distance: String = "euclidean",
  method: String = "ward.D2",
  index: String = "kl"
) extends AbstractRInterface[Seq[Int]] {

  override val LIBRARIES = Seq("NbClust")

  def process(R: RClient): Seq[Int] = {
    println(s"Running NbClust with ($minNbClust, $maxNbClust)")

    R.set("matrix", matrix.map(_.toArray).toArray)
    call(R)("NbClust", "res", "data" -> "matrix",
      "distance" -> s""""$distance"""", "method" -> s""""$method"""",
      "min.nc" -> minNbClust, "max.nc" -> maxNbClust,
      "index" -> s""""$index"""")
    R.evalI1("res$Best.partition")
  }

}
