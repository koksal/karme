package karme

sealed abstract class Experiment[VT, MT <: Measurement[VT]](
  val names: Seq[String], val measurements: Seq[MT])

case class ContinuousExperiment(
  override val names: Seq[String],
  override val measurements: Seq[ContinuousCellMeasurement]
) extends Experiment[Double, ContinuousCellMeasurement](names, measurements)

case class DiscreteExperiment(
  override val names: Seq[String],
  override val measurements: Seq[DiscreteCellMeasurement]
) extends Experiment[Int, DiscreteCellMeasurement](names, measurements)

sealed abstract class Measurement[T](val id: String, val values: Seq[T])
case class ContinuousCellMeasurement(
  override val id: String, override val values: Seq[Double])
  extends Measurement(id, values)
case class DiscreteCellMeasurement(
  override val id: String, override val values: Seq[Int])
  extends Measurement(id, values)
