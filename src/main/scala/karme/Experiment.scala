package karme

case class ContinuousExperiment(
  names: Seq[String],
  measurements: Seq[ContinuousCellMeasurement]
)

case class ContinuousCellMeasurement(id: String, values: Seq[Double])
