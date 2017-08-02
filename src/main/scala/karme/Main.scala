package karme

import karme.evaluation.ClusterPairExpansion
import karme.evaluation.FunctionIOPairs
import karme.evaluation.PerturbationAnalysis
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex
import karme.printing.IOPairLogger
import karme.printing.SynthesisResultLogger
import karme.store.ClusteringStore
import karme.store.EdgePrecedenceStore
import karme.synthesis.SynthesisResult
import karme.synthesis.Synthesizer
import karme.transformations.EdgePrecedenceProducer
import karme.transformations.InputTransformer
import karme.transformations.TransformResult
import karme.visualization.StateGraphPlotter

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)

    val reporter = new Reporter(opts.reporterOpts)

    val annotationContext = AnnotationContext.fromOptions(opts.annotationOpts)

    val inputTransformer = new InputTransformer(opts.inputTransformerOpts,
      annotationContext)(reporter)

    val TransformResult(graph, sources, clustering, perEdgeClustering) =
      inputTransformer.transform()

    logGraph(graph, sources, annotationContext, reporter)

    new ClusteringStore(opts.reporterOpts.outFolder).store(
      clustering.clusterToMember)

    if (opts.runPrecedence) {
      val edgePrecedences = new EdgePrecedenceProducer(graph,
        perEdgeClustering).computePrecedence
      new EdgePrecedenceStore(opts.reporterOpts.outFolder).store(edgePrecedences)
    }

    if (opts.runSynthesis) {
      runSynthesis(opts, graph, sources, clustering, reporter)
    }
  }

  def runSynthesis(
    opts: Opts,
    directedStateGraph: DirectedBooleanStateGraph,
    sources: Set[StateGraphVertex],
    clustering: Clustering,
    reporter: Reporter
  ): Unit = {
    val synthesizer = new Synthesizer(opts.synthOpts, reporter)

    val results = synthesizer.synthesizeForPositiveHardConstraints(
      directedStateGraph)

    val functionsToEvaluate = results map {
      case (name, results) => name -> results.head.functions.head
    }
    val initialStates = sources.map(_.state)
    val perturbationAnalysis = new PerturbationAnalysis(functionsToEvaluate,
      directedStateGraph, initialStates, clustering, Set())(reporter)
    perturbationAnalysis.findGeneDrivers()

    SynthesisResultLogger(results, reporter.file("functions.txt"))

    logIOPairs(results, clustering.clusterToMember, reporter)
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

    IOPairLogger.logPairs(clusterIOPairs, reporter.file("cluster-io-pairs.csv"))
    IOPairLogger.logPairs(geneIOPairs, reporter.file("gene-io-pairs.csv"))
  }

  def logGraph(
    graph: DirectedBooleanStateGraph,
    sources: Set[StateGraphVertex],
    annotationContext: AnnotationContext,
    reporter: Reporter
  ): Unit = {
    new StateGraphPlotter(reporter).plotDirectedGraph(
      graph,
      "state-graph",
      cellClustering = annotationContext.cellClustering,
      nodeHighlightGroups = List(sources.map(_.state))
    )
  }
}
