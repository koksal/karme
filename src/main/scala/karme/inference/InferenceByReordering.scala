package karme.inference

import karme._

object InferenceByReordering {
  def infer(exp: DiscreteExperiment, reporter: FileReporter): Unit = {
    val orderedMs = exp.measurements.sortBy(_.pseudotime)
    val orderedExp = exp.copy(measurements = orderedMs)

    val groupSizeRange = List(100, 200, 400, 800)
    val lagRange = List(-500, -250, -100, 0, 100, 250, 500)

    for {
      groupSize <- groupSizeRange
      lag <- lagRange
    } {
      val scoresByGroup = LaggedGroupedInference.infer(orderedExp, lag, groupSize = Some(groupSize))
      val fn = s"inference-by-reordering-groupsize-${groupSize}-lag-${lag}.csv"
      FunChisq.writeFunChisqResults(reporter.outFile(fn), scoresByGroup)
    }
  }
}
