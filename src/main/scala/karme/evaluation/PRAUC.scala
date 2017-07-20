package karme.evaluation

import java.io.File

import karme.external.AbstractRInterface

class PRAUC extends AbstractRInterface {

  override def LIBRARIES = Seq("PRROC")

  def run(
    positiveScores: Seq[Int], negativeScores: Seq[Int], plotFile: Option[File]
  ): Double = {
    R.set("pos", positiveScores.toArray)
    R.set("neg", negativeScores.toArray)

    R.eval("result <- pr.curve(pos, neg, curve = TRUE)")

    plotFile map { f =>
      R.eval(s"""pdf("${f.getPath()}")""")
      R.eval("plot(result)")
      R.eval("dev.off()")
    }

    R.evalD0("result$auc.integral")
  }
}
