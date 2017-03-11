package karme.clustering

import org.ddahl.rscala.RClient

object NbClustInterface {

  def nbClust(xss: Seq[Seq[Double]]): Int = {
    val R = RClient()
    R eval "library(NbClust)"
    R.set("xss", xss.map(_.toArray).toArray)
    ???
  }

}
