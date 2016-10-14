package karme

case class ContinuousExperiment(
  names: Seq[String],
  cells: Seq[ContinuousCellMeasurement]
)

case class ContinuousCellMeasurement(id: String, values: Seq[Double])
