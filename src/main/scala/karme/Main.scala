package karme

import java.io.File

import karme.evaluation.enrichr.EnrichrPredictionLibrary
import karme.evaluation.enrichr.PredictionEvaluator
import karme.graphs.StateGraphs
import karme.printing.SynthesisResultLogger
import karme.synthesis.Synthesizer
import karme.transformations.InputTransformer
import karme.visualization.StateGraphPlotter

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)

    val reporter = new Reporter(opts.reporterOpts)

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

    val referencePValuePairs = predictionEvaluator.computeReferencePValues(
      optimalResults)

    printRunSummary(opts, referencePValuePairs, reporter.file("summary.tsv"))

    // TODO move to visualization phase module
    val graphPlotter = new StateGraphPlotter(reporter)
    graphPlotter.plotDirectedGraph(directedStateGraph, "directed-state-graph",
      annotationContext.cellClustering, List(initialStates))

    for ((result, i) <- optimalResults.zipWithIndex) {
      SynthesisResultLogger(result, reporter.file(s"functions-$i.txt"))
    }
  }

  def printRunSummary(
    opts: Opts, refPValues: Seq[(EnrichrPredictionLibrary, Double)], f: File
  ): Unit = {
    val optHeaderToValue = Seq(
      "pseudolog" -> opts.inputTransformerOpts.pseudoLogFactor,
      "bool-norm" -> opts.inputTransformerOpts.booleanNormalizationMethod,
      "cell-activity" -> opts.inputTransformerOpts.cellActivityThreshold,
      "uncertainty" -> opts.inputTransformerOpts.uncertaintyThreshold,
      "smoothing" -> opts.inputTransformerOpts.smoothingRadius,
      "minClust" -> opts.inputTransformerOpts.clusteringOpts.minNbClusters,
      "maxClust" -> opts.inputTransformerOpts.clusteringOpts.maxNbClusters
    )
    val refHeaderToValue = refPValues map {
      case (library, pValue) => library.id -> pValue
    }
    val allValues = optHeaderToValue ++ refHeaderToValue

    println(allValues.map(_._1).mkString("\t"))
    println(allValues.map(_._2).mkString("\t"))
  }

}
