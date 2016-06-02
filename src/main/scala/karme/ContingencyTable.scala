package karme

case class ContingencyTable(x: String, y: String, table: Seq[Seq[Int]])
object ContingencyTable {
  def fromExp(exp: DiscreteExperiment): Seq[ContingencyTable] = {
    val prots = exp.measuredProteins
    val protVectors = exp.measurements.map(_.values).transpose

    for {
      (p1, i1) <- prots.zipWithIndex
      (p2, i2) <- prots.zipWithIndex
      if i1 != i2
    } yield {
      val ps = protVectors(i1).zip(protVectors(i2))
      ContingencyTable(p1, p2, fromPairs(ps))
    }
  }

  def fromPairs(ps: Seq[(Int, Int)]): Seq[Seq[Int]] = {
    val xValues = ps.map(_._1).distinct.sorted
    val yValues = ps.map(_._2).distinct.sorted

    xValues map { x =>
      yValues map { y =>
        ps.count(p => p._1 == x && p._2 == y)
      }
    }
  }
}
