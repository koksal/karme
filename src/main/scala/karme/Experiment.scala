package karme

case class ContinuousExperiment(
  names: Seq[String],
  measurements: Seq[ContinuousCellMeasurement]
)

case class DiscreteExperiment(
  names: Seq[String],
  measurements: Seq[DiscreteCellMeasurement]
)

case class ContinuousCellMeasurement(id: String, values: Seq[Double])
case class DiscreteCellMeasurement(id: String, values: Seq[Int])
