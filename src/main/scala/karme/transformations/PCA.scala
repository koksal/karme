package karme.transformations

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

class PCA(
  matrix: Seq[Seq[Double]]
) extends AbstractRInterface[Seq[Seq[Double]]] {

  def process(R: RClient): Seq[Seq[Double]] = {
    R.set("matrix", matrix.map(_.toArray).toArray)
    R.eval("pca <- prcomp(matrix, center = TRUE, scale. = TRUE)")
    ???
  }

}
