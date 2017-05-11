package karme

import karme.evaluation.EvaluationContext
import karme.evaluation.enrichr.EnrichrPredictionLibrary
import karme.evaluation.enrichr.PredictionEvaluator
import karme.graphs.Graphs.Backward
import karme.graphs.Graphs.EdgeDirection
import karme.graphs.Graphs.Forward
import karme.graphs.Graphs.UnlabeledEdge
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphOps
import karme.graphs.StateGraphs.StateGraphVertex
import karme.graphs.StateGraphs.UndirectedStateGraphOps
import karme.printing.SummaryLogger
import karme.printing.SynthesisResultLogger
import karme.synthesis.Synthesizer
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.transformations.InputTransformer
import karme.visualization.StateGraphPlotter

import scala.util.Random

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)

    val reporter = new Reporter(opts.reporterOpts)

    val annotationContext = AnnotationContext.fromOptions(opts.annotationOpts)
    val inputTransformer = new InputTransformer(opts.inputTransformerOpts,
      annotationContext, reporter)

    val (directedStateGraph, initialStates) =
      inputTransformer.buildDirectedStateGraph()

    val clustering = inputTransformer.getClustering().get

    new StateGraphPlotter(reporter).plotDirectedGraph(directedStateGraph,
      "directed-state-graph", nodeHighlightGroups = List(initialStates))

    plotGraphWithReferences(opts, directedStateGraph, initialStates,
      clustering, reporter)

    if (opts.runSynthesis) {
      runSynthesis(opts, inputTransformer, directedStateGraph, initialStates,
        reporter)
    }
  }

  def plotGraphWithReferences(
    opts: Opts,
    graph: DirectedBooleanStateGraph,
    initialStates: Set[ConcreteBooleanState],
    clustering: Map[String, Set[String]],
    reporter: Reporter
  ): Unit = {
    val evalContext = EvaluationContext.fromOptions(opts.evalOpts)

    for (reference <- evalContext.references) {
      plotGraphWithReference(graph, initialStates, clustering, reference,
        reporter)
    }
  }

  def plotGraphWithReference(
    graph: DirectedBooleanStateGraph,
    initialStates: Set[ConcreteBooleanState],
    clustering: Map[String, Set[String]],
    reference: EnrichrPredictionLibrary,
    reporter: Reporter
  ): Unit = {

    // take reference pairs
    val refPairs = PredictionEvaluator.referencePairs(reference)

    // aggregate all clustered names in graph
    val clusteredNames = clustering.values.flatten.toSet

    // filter reference down to clustered names
    val refPairsInGraph = refPairs filter {
      case (src, tgt) => {
        clusteredNames.contains(src) && clusteredNames.contains(tgt)
      }
    }

    val randomRefSample = new Random().shuffle(refPairsInGraph.toList).take(10)

    var augmentedGraph = graph
    var newEdges = Set[(UnlabeledEdge[StateGraphVertex], EdgeDirection)]()
    // compute reference edges:
    // draw an edge from a label landing state for a target to a label landing
    // state for a source

    // find all edges that have the target label
    // find landing states for those edges
    // do the same for the source label
    for ((src, tgt) <- randomRefSample) {
      println(s"reference edge: ${(src, tgt)}")
      val edgesWithSourceLabel = clusterLevelEdgesForGeneName(graph,
        clustering, src)

      val edgesWithTargetLabel = clusterLevelEdgesForGeneName(graph,
        clustering, tgt)

      val graphSources = edgesWithSourceLabel.flatMap(e => graph.targets(e))
      val graphTargets = edgesWithTargetLabel.flatMap(e => graph.targets(e))

      println(s"Graph sources: $graphSources")
      println(s"Graph targets: $graphTargets")

      for {
        graphSrc <- graphSources
        graphTgt <- graphTargets
      } {
        augmentedGraph = augmentedGraph.addEdge(graphSrc, graphTgt)

        if (graphSrc < graphTgt) {
          newEdges += ((UnlabeledEdge(graphSrc, graphTgt), Forward))
        } else {
          newEdges += ((UnlabeledEdge(graphTgt, graphSrc), Backward))
        }
      }
    }

    new StateGraphPlotter(reporter).plotDirectedGraph(augmentedGraph,
      s"directed-state-graph-with-${reference.id}",
      nodeHighlightGroups = List(initialStates),
      edgeHighlightGroups = List(newEdges))

  }

  def clusterLevelEdgesForGeneName(
    graph: DirectedBooleanStateGraph,
    clustering: Map[String, Set[String]],
    geneName: String
  ): Set[UnlabeledEdge[StateGraphVertex]] = {
    val cluster = clustering.find{
      case (cluster, members) => members.contains(geneName)
    }.get._1
    graph.E filter { e =>
      UndirectedStateGraphOps.edgeLabels(e).contains(cluster)
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
