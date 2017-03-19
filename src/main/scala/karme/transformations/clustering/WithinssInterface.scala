package karme.transformations.clustering

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

class WithinssInterface(
  xss: Seq[Seq[Double]]
) extends AbstractRInterface[Double] {

  def process(R: RClient): Double = {
    R.set("xss", xss.map(_.toArray).toArray)
    R.eval("res = kmeans(xss, 1)")
    R.evalD0("res$withinss")
  }

}
