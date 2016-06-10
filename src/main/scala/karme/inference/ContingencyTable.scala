package karme.inference

import karme.DiscreteExperiment

object ContingencyTable {
  type ContingencyTable = Seq[Seq[Int]]

  def fromPairs(xValues: Seq[Int], yValues: Seq[Int], ps: Seq[(Int, Int)]): ContingencyTable = {
    xValues map { x =>
      yValues map { y =>
        ps.count(p => p._1 == x && p._2 == y)
      }
    }
  }
}
