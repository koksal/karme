package karme.transformations.clustering

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

class NbClustInterface extends AbstractRInterface {

  override def LIBRARIES = Seq("NbClust")

  def cluster(
    matrix: Seq[Seq[Double]],
    minNbClust: Int,
    maxNbClust: Int,
    distance: String = "euclidean",
    method: String = "kmeans",
    index: String = "kl"
  ): Seq[Int] = {
    println(s"Running NbClust with ($minNbClust, $maxNbClust)")

    R.set("matrix", matrix.map(_.toArray).toArray)
    call("NbClust", "res", "data" -> "matrix",
      "distance" -> s""""$distance"""", "method" -> s""""$method"""",
      "min.nc" -> minNbClust, "max.nc" -> maxNbClust,
      "index" -> s""""$index"""")
    R.evalI1("res$Best.partition")
  }

}
