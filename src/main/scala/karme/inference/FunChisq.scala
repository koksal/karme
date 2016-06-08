package karme.inference

import karme.DiscreteExperiment
import karme.RInterface

case class FunChisqResult(statistic: Double, pValue: Double, estimate: Double) {
  override def toString: String = s"Statistic: $statistic, p-value: $pValue, estimate: $estimate"
}

object FunChisq {
  def scores(exp: DiscreteExperiment): Map[(String, String), FunChisqResult] = {
    val contTables = ContingencyTable.fromExp(exp)

    val ss = for (ct <- contTables) yield {
      // temporarily write out contingency tables
      val x = ct.x
      val y = ct.y
      val f = new java.io.File(s"$x-$y-contingency.csv")
      karme.FileReporter.writeMatrix(f, ct.table)

      val res = RInterface.funChisq(ct)
      (ct.x, ct.y) -> res
    }
    ss.toMap
  }
}
