package karme

import karme.graphs.StateGraphs
import karme.synthesis.Synthesizer
import karme.transformations.SynthesisInputBuilder

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)

    val reporter = new Reporter(opts.outFolder)

    val synthesisInputBuilder = new SynthesisInputBuilder(
      opts.synthInputBuilderOpts)

    val synthesizer = new Synthesizer(opts.synthOpts, reporter)

    val directedStateGraph = synthesisInputBuilder.buildDirectedStateGraph()
    val initialStates = StateGraphs.initialTrajectoryStates(
      directedStateGraph.V, synthesisInputBuilder.trajectories)

    val synthesisResults = synthesizer.synthesizeForOptimalReachability(
      directedStateGraph, initialStates)
  }

}
