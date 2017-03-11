package karme.clustering

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

object NbClustInterface extends
  AbstractRInterface[Seq[Seq[Double]], Seq[Int]] {

  val libraries = Seq("NbClust")

  def process(R: RClient)(xss: Seq[Seq[Double]]): Seq[Int] = {
    R.set("xss", xss.map(_.toArray).toArray)
    R.eval("res = NbClust(xss)")
    R.evalI1("res$Best.partition")
  }

}
