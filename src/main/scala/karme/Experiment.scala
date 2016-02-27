package karme

case class Experiment(measuredProteins: Seq[String], measurements: IndexedSeq[CellMeasurement]) {
  def toTuples(): Seq[Map[String, String]] = {
    val maps = measurements map { m =>
      val ms = measuredProteins.zipWithIndex map { 
        case (mp, i) => mp -> m.values(i).toString 
      }
      val metadata = Map("Minute" -> m.time.toString)
      ms.toMap ++ metadata
    }
    maps
  }

  def toTuplesWithPseudotime(ps: Array[Double]): Seq[Map[String, String]] = {
    this.toTuples().zipWithIndex map {
      case (t, i) => t + ("Pseudotime" -> ps(i).toString)
    }
  }
}

case class CellMeasurement(time: Double, values: IndexedSeq[Double])
