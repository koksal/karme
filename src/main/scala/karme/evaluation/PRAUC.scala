package karme.evaluation

import java.io.File

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

class PRAUC(
  positiveScores: Seq[Int], negativeScores: Seq[Int], plotFile: File
) extends AbstractRInterface[Double] {

  override val LIBRARIES = Seq("PRROC")

  def process(R: RClient): Double = {
    R.set("pos", positiveScores.toArray)
    R.set("neg", negativeScores.toArray)

    R.eval("result <- pr.curve(pos, neg, curve = TRUE)")

    R.eval(s"""pdf("${plotFile.getPath()}")""")
    R.eval("plot(result)")
    R.eval("dev.off()")

    val seq = R.evalD2("result$curve").map(_.toSeq).toSeq
    println(seq.mkString("\n"))

    R.evalD0("result$auc.integral")
  }
}
