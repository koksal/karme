package karme.clustering

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

class NbClustInterface(matrix: Seq[Seq[Double]]) extends
  AbstractRInterface[Seq[Int]] {

  val minNbClust = 10
  val maxNbClust = 30
  val index = "ch"

  val libraries = Seq("NbClust")

  def process(R: RClient): Seq[Int] = {
    R.set("matrix", matrix.map(_.toArray).toArray)
    call(R)("NbClust", "res", "data" -> "matrix", "distance" -> "\"euclidean\"",
      "method" -> "\"complete\"", "min.nc" -> minNbClust,
      "max.nc" -> maxNbClust, "index" -> s""""$index"""")
    println(R.evalS0("res$Best.nc"))
    R.evalI1("res$Best.partition")
  }

}
