package karme.transformations.clustering

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

class WithinssInterface extends AbstractRInterface {

  def run(xss: Seq[Seq[Double]]): Double = {
    R.set("xss", xss.map(_.toArray).toArray)
    R.eval("res = kmeans(xss, 1)")
    R.evalD0("res$withinss")
  }

}
