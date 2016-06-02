package karme

trait AbsExperiment[T] {
  val measuredProteins: Seq[String]
  val measurements: IndexedSeq[AbsCellMeasurement[T]]
  lazy val samplingTimes: Seq[Double] = measurements.map(_.time).distinct.sorted

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

  def toFlattenedTuples(expName: String, processName: String): Seq[Map[String, String]] = {
    measurements.flatMap{ m =>
      measuredProteins.zipWithIndex.map{ case(p, i) =>
        Map(
          "pseudotime" -> m.pseudotime.toString,
          "value" -> m.values(i).toString,
          "protein" -> p,
          "experiment" -> expName,
          "process" -> processName
        )
      }
    }
  }
}

case class Experiment(
  measuredProteins: Seq[String], 
  measurements: IndexedSeq[CellMeasurement]
) extends AbsExperiment[Double]

case class DiscreteExperiment(
  measuredProteins: Seq[String], 
  measurements: IndexedSeq[DiscreteCellMeasurement],
  discretizationLevels: IndexedSeq[Int]
) extends AbsExperiment[Int]

trait AbsCellMeasurement[T] {
  val time: Double
  val actualTime: Double
  val pseudotime: Double
  val values: IndexedSeq[T]
}

case class CellMeasurement(
  time: Double, 
  actualTime: Double, 
  pseudotime: Double, 
  values: IndexedSeq[Double]
) extends AbsCellMeasurement[Double]

case class DiscreteCellMeasurement(
  time: Double, 
  actualTime: Double, 
  pseudotime: Double, 
  values: IndexedSeq[Int]
) extends AbsCellMeasurement[Int]
