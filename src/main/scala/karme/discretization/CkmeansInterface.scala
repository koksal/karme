package karme.discretization

import org.ddahl.rscala.RClient

object CkmeansInterface {
  def ckmeans(xs: Seq[Double]): Seq[Int] = {
    val R = RClient()
    R.eval("library(Ckmeans.1d.dp)")

    R.set("xs", xs.toArray)
    R.eval(s"k = c(${Discretization.LOW_VALUE}, ${Discretization.HIGH_VALUE})")
    R.eval(s"result <- Ckmeans.1d.dp(xs, k = k)")
    R.evalI1("result$cluster")
  }
}
