package karme.discretization

import org.ddahl.rscala.RClient

object CkmeansInterface {
  def ckmeans(xs: Seq[Double]): Seq[Int] = {
    // TODO consider moving to preprocess
    // TODO consider lifting imports to an RInterface parent class
    val R = RClient()
    R.eval("library(Ckmeans.1d.dp)")

    R.set("xs", xs.toArray)
    R.eval("result <- Ckmeans.1d.dp(xs, k = c(1, 2))")
    R.evalI1("result$cluster")
  }
}
