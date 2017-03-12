package karme.clustering

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

class NbClustInterface(
  matrix: Seq[Seq[Double]],
  minNbClust: Int,
  maxNbClust: Int,
  index: String
) extends AbstractRInterface[Seq[Int]] {

  override val LIBRARIES = Seq("NbClust")

  val METHOD = "ward.D2"

  def process(R: RClient): Seq[Int] = {
    println(s"Running NbClust with ($minNbClust, $maxNbClust)")

    R.set("matrix", matrix.map(_.toArray).toArray)
    call(R)("NbClust", "res", "data" -> "matrix", "distance" -> "\"euclidean\"",
      "method" -> s""""$METHOD"""", "min.nc" -> minNbClust,
      "max.nc" -> maxNbClust, "index" -> s""""$index"""")
    R.evalI1("res$Best.partition")
  }

}
