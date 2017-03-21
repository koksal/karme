package karme

import karme.graphs.StateGraphs
import karme.synthesis.Synthesizer
import karme.transformations.InputTransformer

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)

    val reporter = new Reporter(opts.outFolder)

    val annotationContext = AnnotationContext.fromOptions(opts.annotationOpts)
    val synthesisInputBuilder = new InputTransformer(
      opts.inputTransformerOpts, annotationContext)

    val synthesizer = new Synthesizer(opts.synthOpts, reporter)

    val directedStateGraph = synthesisInputBuilder.buildDirectedStateGraph()
    val initialStates = StateGraphs.initialTrajectoryStates(
      directedStateGraph.V, synthesisInputBuilder.trajectories)

    val optimalResults = synthesizer.synthesizeForOptimalReachability(
      directedStateGraph, initialStates)
  }

}
