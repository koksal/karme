package karme

import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.printing.SynthesisResultLogger
import karme.synthesis.Synthesizer
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.transformations.InputTransformer

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)

    val reporter = new Reporter(opts.reporterOpts)

    val annotationContext = AnnotationContext.fromOptions(opts.annotationOpts)
    val inputTransformer = new InputTransformer(opts.inputTransformerOpts,
      annotationContext, reporter)

    val directedStateGraph = inputTransformer.buildDirectedStateGraph()
    val initialStates = StateGraphs.initialTrajectoryStates(
      directedStateGraph.V, inputTransformer.trajectories)

    if (opts.runSynthesis) {
      runSynthesis(opts, inputTransformer, directedStateGraph, initialStates,
        reporter)
    }
  }

  def runSynthesis(
    opts: Opts,
    inputTransformer: InputTransformer,
    directedStateGraph: DirectedBooleanStateGraph,
    initialStates: Set[ConcreteBooleanState],
    reporter: Reporter
  ): Unit = {

    val synthesizer = new Synthesizer(opts.synthOpts, reporter)

    val results = synthesizer.synthesizeForPositiveHardConstraints(
      directedStateGraph)

    /**
    val predictionEvaluator = new PredictionEvaluator(opts.evalOpts,
      inputTransformer.getNamesBeforeFiltering(),
      inputTransformer.getClustering().get, reporter)

    val referencePValuePairs = predictionEvaluator.computeReferencePValues(
      optimalResults.map(_.labelToResult))

    SummaryLogger(opts, optimalResults, referencePValuePairs,
      reporter.file("summary.tsv"))
      */

    SynthesisResultLogger(results, reporter.file("functions.txt"))
  }
}
