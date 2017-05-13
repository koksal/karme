package karme

import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.printing.SynthesisResultLogger
import karme.synthesis.Synthesizer
import karme.transformations.InputTransformer
import karme.visualization.StateGraphPlotter

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)

    val reporter = new Reporter(opts.reporterOpts)

    val annotationContext = AnnotationContext.fromOptions(opts.annotationOpts)

    val inputTransformer = new InputTransformer(opts.inputTransformerOpts,
      annotationContext, reporter)

    val (graph, sources) = inputTransformer.buildStateGraphAndSources

    // TODO get initial graph states and print graph
    new StateGraphPlotter(reporter).plotDirectedGraph(graph, "state-graph",
      nodeHighlightGroups = List(sources.map(_.state)))

    if (opts.runSynthesis) {
      runSynthesis(opts, inputTransformer, graph, reporter)
    }
  }

  def runSynthesis(
    opts: Opts,
    inputTransformer: InputTransformer,
    directedStateGraph: DirectedBooleanStateGraph,
    reporter: Reporter
  ): Unit = {
    val synthesizer = new Synthesizer(opts.synthOpts, reporter)

    val results = synthesizer.synthesizeForPositiveHardConstraints(
      directedStateGraph)

    SynthesisResultLogger(results, reporter.file("functions.txt"))
  }
}
