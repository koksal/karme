package karme.evaluation

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

class PRAUC(
  positiveScores: Seq[Int], negativeScores: Seq[Int]
) extends AbstractRInterface[Double] {

  override val LIBRARIES = Seq("PRROC")

  def process(R: RClient): Double = {
    R.set("pos", positiveScores.toArray)
    R.set("neg", negativeScores.toArray)

    R.eval("result <- pr.curve(pos, neg)")

    R.evalD0("result$auc.integral")
  }
}
