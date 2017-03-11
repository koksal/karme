package karme.clustering

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

class NbClustInterface(matrix: Seq[Seq[Double]]) extends
  AbstractRInterface[Seq[Int]] {

  val libraries = Seq("NbClust")

  def process(R: RClient): Seq[Int] = {
    R.set("matrix", matrix.map(_.toArray).toArray)
    R.eval("res = NbClust(matrix)")
    R.evalI1("res$Best.partition")
  }

}
