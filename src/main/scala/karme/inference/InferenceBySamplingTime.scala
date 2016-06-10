package karme.inference

import karme._

object InferenceBySamplingTime {
  def infer(exp: DiscreteExperiment, reporter: FileReporter): Seq[Seq[FunChisqResult]] = {
    val scoresByTime = for (t <- exp.samplingTimes) yield {
      val msAtT = exp.measurements.filter(_.time == t)
      val expAtT = exp.copy(measurements = msAtT)
      val scores = LaggedGroupedInference.infer(expAtT, lag = 0, groupSize = None)
      scores.flatten
    }
    FunChisq.writeFunChisqResults(reporter.outFile("inference-by-sampling-time-scores.csv"), scoresByTime)
    scoresByTime
  }
}
