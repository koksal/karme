package karme.inference

import karme._

object InferenceByReordering {
  def infer(exp: DiscreteExperiment, reporter: FileReporter): Seq[Seq[FunChisqResult]] = {
    val groupSize = 500
    val orderedMs = exp.measurements.sortBy(_.pseudotime)
    val orderedExp = exp.copy(measurements = orderedMs)
    val scoresByGroup = LaggedGroupedInference.infer(orderedExp, lag = 0, groupSize = Some(groupSize))
    FunChisq.writeFunChisqResults(reporter.outFile("inference-by-reordering-scores.csv"), scoresByGroup)
    scoresByGroup
  }
}
