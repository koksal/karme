package karme

abstract class Experiment[A, B <: Measurement[A]](
  names: Seq[String], measurements: Seq[A])

case class ContinuousExperiment(
  names: Seq[String],
  measurements: Seq[ContinuousCellMeasurement]
) extends Experiment(names, measurements)

case class DiscreteExperiment(
  names: Seq[String],
  measurements: Seq[DiscreteCellMeasurement]
) extends Experiment(names, measurements)

abstract class Measurement[T](id: String, values: Seq[T])
case class ContinuousCellMeasurement(id: String, values: Seq[Double])
  extends Measurement(id, values)
case class DiscreteCellMeasurement(id: String, values: Seq[Int])
  extends Measurement(id, values)
