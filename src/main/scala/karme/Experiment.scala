package karme

case class Experiment(measuredProteins: Seq[String], measurements: IndexedSeq[CellMeasurement]) {
  def toTuples(): Seq[Map[String, String]] = {
    val maps = measurements map { m =>
      val ms = measuredProteins.zipWithIndex map { 
        case (mp, i) => mp -> m.values(i).toString 
      }
      val metadata = Map( 
        "Minute" -> m.time.toString,
        "ActualTime" -> m.actualTime.toString,
        "Pseudotime" -> m.pseudotime.toString
      )
      ms.toMap ++ metadata
    }
    maps
  }

  def toFlattenedTuples(): Seq[Map[String, String]] = {
    measurements.flatMap{ m =>
      measuredProteins.zipWithIndex.map{ case(p, i) =>
        Map(
          "Pseudotime" -> m.pseudotime.toString,
          "Value" -> m.values(i).toString,
          "Group" -> p
        )
      }
    }
  }
}

case class CellMeasurement(time: Double, actualTime: Double, pseudotime: Double, values: IndexedSeq[Double])
