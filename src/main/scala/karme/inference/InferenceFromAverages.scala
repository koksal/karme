package karme.inference

import karme._

object InferenceFromAverages {
  def infer(exp: Experiment, reporter: FileReporter): Unit = {
    val avgExp = Transformations.averageBySamplingTime(exp)
    val avgDiscrExp = discretization.Discretization.discretizeExperiment(avgExp)
    Util.writeExp(reporter, avgExp, "average-experiment.csv")
    Util.writeExp(reporter, avgDiscrExp, "discrete-average-experiment.csv")

    val lagRange = -2 to 2
    lagRange map (lag => infer(avgDiscrExp, reporter, lag))
  }

  private def infer(exp: DiscreteExperiment, reporter: FileReporter, lag: Int): Unit = {
    val avgScore = LaggedGroupedInference.infer(exp, lag, groupSize = None).flatten

    FunChisq.writeFunChisqResults(reporter.outFile(s"inference-by-averaging-scores-lag-$lag.csv"), List(avgScore))
  }
}
