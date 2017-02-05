package karme.clustering

import org.ddahl.rscala.RClient

object KmeansInterface {

  def withinSumOfSquares(xss: Seq[Seq[Double]]): Double = {
    val R = RClient()
    R.set("xss", xss)
    R.eval("res = kmeans(xss, 1)")
    R.evalD0("res$withinss")
  }

}
