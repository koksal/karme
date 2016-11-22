package karme.discretization

import org.ddahl.rscala.RClient

object CkmeansInterface {
  def ckmeans(xs: Seq[Double]): Seq[Int] = {
    println(s"Running ckmeans for ${xs.size} values.")
    // TODO consider moving to preprocess
    // TODO consider lifting imports to an RInterface parent class
    val R = RClient()
    R.eval("library(Ckmeans.1d.dp)")

    R.set("xs", xs.toArray)
    R.eval(s"result <- Ckmeans.1d.dp(xs, k = c(${Discretization.LOW_VALUE}, " +
      s"${Discretization.HIGH_VALUE}))")
    R.evalI1("result$cluster")
  }
}
