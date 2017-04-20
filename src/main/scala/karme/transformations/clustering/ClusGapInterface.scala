package karme.transformations.clustering

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

class ClusGapInterface(
  matrix: Seq[Seq[Double]],
  maxK: Int
) extends AbstractRInterface[Seq[Int]] {

  override val LIBRARIES = Seq("cluster")

  override def process(R: RClient): Seq[Int] = {
    R.set("matrix", matrix.map(_.toArray).toArray)
    R.eval(s"gap <- clusGap(matrix, kmeans, K.max=$maxK, B=500)")
    R.eval(
      """k <- maxSE(
        |gap$Tab[, "gap"],
        |gap$Tab[, "SE.sim"],
        |method="Tibs2001SEmax")""".stripMargin)
    R.eval("fit <- kmeans(matrix, k)")
    R.evalI1("fit$cluster")
  }

}
