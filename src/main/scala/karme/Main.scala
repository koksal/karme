package karme

import karme.evaluation.enrichr.PredictionEvaluator
import karme.graphs.StateGraphs
import karme.printing.SynthesisResultLogger
import karme.synthesis.Synthesizer
import karme.transformations.InputTransformer
import karme.visualization.StateGraphPlotter

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)

    val reporter = new Reporter(opts.outFolder)

    val annotationContext = AnnotationContext.fromOptions(opts.annotationOpts)
    val inputTransformer = new InputTransformer(
      opts.inputTransformerOpts, annotationContext, reporter)

    val synthesizer = new Synthesizer(opts.synthOpts, reporter)

    val directedStateGraph = inputTransformer.buildDirectedStateGraph()
    val initialStates = StateGraphs.initialTrajectoryStates(
      directedStateGraph.V, inputTransformer.trajectories)

    val optimalResults = synthesizer.synthesizeForOptimalReachability(
      directedStateGraph, initialStates)

    val predictionEvaluator = new PredictionEvaluator(opts.evalOpts,
      inputTransformer.getNamesBeforeFiltering(),
      inputTransformer.getClustering().get)

    predictionEvaluator.compareToReferences(optimalResults)

    // TODO move to visualization phase module
    val graphPlotter = new StateGraphPlotter(reporter)
    graphPlotter.plotDirectedGraph(directedStateGraph, "directed-state-graph",
      annotationContext.cellClustering, List(initialStates))

    for ((result, i) <- optimalResults.zipWithIndex) {
      SynthesisResultLogger(result, reporter.file(s"functions-$i.txt"))
    }
  }

}
