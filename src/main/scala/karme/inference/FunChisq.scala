package karme.inference

import karme.DiscreteExperiment
import karme.RInterface

object FunChisq {
  def scores(exp: DiscreteExperiment): Map[(String, String), Double] = {
    val contTables = ContingencyTable.fromExp(exp)

    val ss = for (ct <- contTables) yield {
      val score = RInterface.funChisq(ct)
      (ct.x, ct.y) -> score
    }
    ss.toMap
  }
}
