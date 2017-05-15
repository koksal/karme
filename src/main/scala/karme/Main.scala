package karme

import karme.evaluation.ClusterPairExpansion
import karme.evaluation.FunctionIOPairs
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.printing.IOPairLogger
import karme.printing.SynthesisResultLogger
import karme.synthesis.SynthesisResult
import karme.synthesis.Synthesizer
import karme.transformations.InputTransformer
import karme.transformations.TransformResult
import karme.visualization.StateGraphPlotter

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)

    val reporter = new Reporter(opts.reporterOpts)

    val annotationContext = AnnotationContext.fromOptions(opts.annotationOpts)

    val inputTransformer = new InputTransformer(opts.inputTransformerOpts,
      annotationContext, reporter)

    val TransformResult(graph, sources, clustering) =
      inputTransformer.transform()

    new StateGraphPlotter(reporter).plotDirectedGraph(graph, "state-graph",
      nodeHighlightGroups = List(sources.map(_.state)))

    if (opts.runSynthesis) {
      runSynthesis(opts, inputTransformer, graph, clustering, reporter)
    }
  }

  def runSynthesis(
    opts: Opts,
    inputTransformer: InputTransformer,
    directedStateGraph: DirectedBooleanStateGraph,
    clustering: Map[String, Set[String]],
    reporter: Reporter
  ): Unit = {
    val synthesizer = new Synthesizer(opts.synthOpts, reporter)

    val results = synthesizer.synthesizeForPositiveHardConstraints(
      directedStateGraph)

    SynthesisResultLogger(results, reporter.file("functions.txt"))

    logIOPairs(results, clustering, reporter)
  }

  def logIOPairs(
    results: Map[String, Set[SynthesisResult]],
    clustering: Map[String, Set[String]],
    reporter: Reporter
  ): Unit = {
    var clusterIOPairs = Seq[(String, String)]()
    for ((label, labelResults) <- results) {
      for (res <- labelResults) {
        clusterIOPairs ++= FunctionIOPairs.funInputOutputPairs(label, res)
      }
    }

    val geneIOPairs = new ClusterPairExpansion(
      clustering).clusterMemberPairs(clusterIOPairs)

    IOPairLogger(clusterIOPairs, reporter.file("cluster-io-pairs.csv"))
    IOPairLogger(geneIOPairs, reporter.file("gene-io-pairs.csv"))
  }
}
