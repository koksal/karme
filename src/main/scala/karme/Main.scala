package karme

import karme.evaluation.enrichr.PredictionEvaluator
import karme.graphs.StateGraphs
import karme.synthesis.Synthesizer
import karme.transformations.InputTransformer

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)

    val reporter = new Reporter(opts.outFolder)

    val annotationContext = AnnotationContext.fromOptions(opts.annotationOpts)
    val inputTransformer = new InputTransformer(
      opts.inputTransformerOpts, annotationContext)

    val synthesizer = new Synthesizer(opts.synthOpts, reporter)

    val directedStateGraph = inputTransformer.buildDirectedStateGraph()
    val initialStates = StateGraphs.initialTrajectoryStates(
      directedStateGraph.V, inputTransformer.trajectories)

    val optimalResults = synthesizer.synthesizeForOptimalReachability(
      directedStateGraph, initialStates)

    val predictionEvaluator = new PredictionEvaluator(opts.evalOpts, ???)
    predictionEvaluator.compareToReferences(optimalResults,
      inputTransformer.getClustering())
  }

}
