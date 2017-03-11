package karme.clustering

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

object NbClustInterface extends
  AbstractRInterface[Seq[Seq[Double]], Int] {

  def process(R: RClient)(xss: Seq[Seq[Double]]): Int = {
    R eval "library(NbClust)"
    R.set("xss", xss.map(_.toArray).toArray)
    ???
  }

}
