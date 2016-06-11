package karme.inference

import karme._

object InferenceByReordering {
  def infer(exp: DiscreteExperiment, reporter: FileReporter): Unit = {
    val orderedMs = exp.measurements.sortBy(_.pseudotime)
    val orderedExp = exp.copy(measurements = orderedMs)

    val groupSizeRange = List(100, 500, 1000)
    val lagRange = List(0) // (-2 to 2) map (_ * 1000)

    for {
      groupSize <- groupSizeRange
      lag <- lagRange
    } {
      println(s"Running inference for group size ${groupSize}, lag ${lag}")
      val scoresByGroup = Util.time {
        LaggedGroupedInference.infer(orderedExp, lag, groupSize = Some(groupSize))
      }
      val fn = s"inference-by-reordering-groupsize-${groupSize}-lag-${lag}.csv"
      FunChisq.writeFunChisqResults(reporter.outFile(fn), scoresByGroup)
    }
  }
}
