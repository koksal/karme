package karme.inference

import karme.Experiment
import karme.DiscreteExperiment

object LaggedGroupedInference {
  def infer(
    exp: DiscreteExperiment, 
    lag: Int,
    groupSize: Option[Int]
  ): Seq[Seq[FunChisqResult]] = {
    val prots = exp.measuredProteins
    val protVectors = exp.measurements.map(_.values).transpose

    val res = for {
      (p1, i1) <- prots.zipWithIndex
      (p2, i2) <- prots.zipWithIndex
      if i1 != i2
    } yield {
      var ps1 = protVectors(i1)
      var ps2 = protVectors(i2)
      val xValues = 1 to exp.discretizationLevels(i1)
      val yValues = 1 to exp.discretizationLevels(i2)

      if (lag > 0) ps2 = ps2 drop lag
      if (lag < 0) ps1 = ps1 drop math.abs(lag)
      val ps = ps1 zip ps2
      val groups = groupSize match {
        case None => List(ps)
        case Some(size) => ps.grouped(size).toList
      }
      val results = groups map { group =>
        val score = FunChisq.score(xValues, yValues, group)
        FunChisqResult(p1, p2, lag, score)
      }

      results.toList
    }

    res.transpose
  }
}
