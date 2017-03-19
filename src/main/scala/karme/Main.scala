package karme

import java.io.File

import karme.Experiments.ContinuousExperiment
import karme.transformations.clustering.HierarchicalClustering
import karme.transformations.discretization.Discretization
import karme.evaluation.ReachabilityEvaluation
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.UndirectedStateGraphOps
import karme.parsing.BooleanExperimentParser
import karme.parsing.NamesParser
import karme.parsing.{CellTrajectoryParser, ClusteringParser, ContinuousExperimentParser}
import karme.printing.ExperimentLogger
import karme.printing.StatePseudotimeLogger
import karme.printing.TransitionLogger
import karme.synthesis.Synthesizer
import karme.transformations.ExperimentTransformation
import karme.transformations.TransitionProducer
import karme.transformations.smoothing.BinomialMLE
import karme.util.NamingUtil
import karme.visualization.{CurvePlot, StateGraphVisualization}

import scala.collection.mutable

object Main {

  def main2(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)

    val reporter = new Reporter(opts.outFolder)

    val synthesisInputBuilder = new SynthesisInputBuilder()
  }

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    opts.outFolder.mkdirs()

    val namesToFilter = NamesParser(opts.namesFiles)

    val continuousExperiment = readAndTransformContinuousExperiment(
      opts.continuousExperimentFile.getOrElse(sys.error(
        "No continuous experiment given.")), namesToFilter,
      opts.analysisOptions.pseudoLogFactor)

    val annotationVars = NamingUtil.selectNames(
      continuousExperiment.names.toSet, NamesParser(opts.annotationsFiles))

    val booleanExperiment = opts.discretizedExperimentFile match {
      case Some(f) => {
        BooleanExperimentParser.parseAndFilter(f, None)
      }
      case None => {
        println("Discretizing.")

        // first discretization pass
        val binarized = Discretization.binarize(continuousExperiment,
          opts.analysisOptions.firstDiscretizationMethod)

        println("Filtering.")
        // compute set of variables to filter out
        val variablesWithOneLevel =
          ExperimentTransformation.namesWithSingleValue(binarized)
        val inactiveVars = ExperimentTransformation.inactiveVariables(
          binarized, opts.analysisOptions.cellActivityThreshold)
        var varsToDrop = variablesWithOneLevel ++ inactiveVars
        if (opts.analysisOptions.forceAnnotations) {
          varsToDrop = varsToDrop -- annotationVars
        }
        val filtered = binarized.project(binarized.names.toSet -- varsToDrop)
        println(s"Filtering Boolean experiment down to ${filtered.names.size}")

        ExperimentLogger.saveToFile(filtered,
          new File(opts.outFolder, "experiment-discretized-filtered.csv"))

        filtered
      }
    }

    println("Reading trajectories.")
    val trajectories = opts.trajectoryFiles map CellTrajectoryParser.parse

    val mleExperiment = opts.mleExperimentFile match {
      case Some(f) => {
        ContinuousExperimentParser.parseAndFilter(f, None)
      }
      case None => {
        println("Computing MLE.")

        val res = BinomialMLE.run(booleanExperiment, trajectories,
          opts.analysisOptions.smoothingRadius)
        ExperimentLogger.saveToFile(res,
          new File(opts.outFolder, "experiment-mle.csv"))
        res
      }
    }

    val clusteredExperiment = if (opts.analysisOptions.cluster) {
      println("Clustering variables.")
      val geneClustering = HierarchicalClustering.clusterVariables(
        mleExperiment, annotationVars,
        opts.analysisOptions.minNbClusters,
        opts.analysisOptions.maxNbClusters, opts.outFolder)
      val clusteredExp = HierarchicalClustering.experimentFromClusterAverages(
        mleExperiment, geneClustering, annotationVars)

      ExperimentLogger.saveToFile(clusteredExp,
        new File(opts.outFolder, "experiment-clustered.csv"))
      if (opts.visualizationOptions.curves) {
        CurvePlot.plotClusterGenes(mleExperiment, trajectories,
          geneClustering, opts.outFolder)
      }

      clusteredExp
    } else {
      mleExperiment
    }

    println("Converting to three-valued states")
    val threeValuedExperiment = Experiments.threeValuedExpFromMixtureModel(
      clusteredExperiment, 0.4)
    ExperimentLogger.saveToFile(threeValuedExperiment,
      new File(opts.outFolder, "experiment-three-valued.csv"))

    val cellClustering = readClustering(opts.cellClusteringFile)

    println("Expanding three-valued experiment to Boolean combinations.")
    val booleanExpFromCombinations = StateGraphs.expandWithBooleanCombinations(
      threeValuedExperiment)

    println("Building graphs.")
    val undirectedStateGraph = StateGraphs.fromBooleanExperiment(
      booleanExpFromCombinations, opts.analysisOptions.maxHammingDistance)
    val directedStateGraph = UndirectedStateGraphOps.orientByTrajectories(
      undirectedStateGraph, trajectories)
    println(s"Produced an undirected state graph with " +
      s"|V| = ${undirectedStateGraph.V.size} and " +
      s"|E| = ${undirectedStateGraph.E.size}")

    if (opts.visualizationOptions.stateGraphs) {
      val initialStates = StateGraphs.initialTrajectoryStates(
        undirectedStateGraph.V, trajectories)
      val highlightGroups = List(initialStates)

      StateGraphVisualization.plotUndirectedGraph(undirectedStateGraph,
        "original", opts.outFolder, cellClustering = cellClustering,
        nodeHighlightGroups = highlightGroups)
      StateGraphVisualization.plotDirectedGraph(directedStateGraph,
        opts.outFolder, cellClustering = cellClustering,
        nodeHighlightGroups = highlightGroups)

      StatePseudotimeLogger.savePseudotimes(undirectedStateGraph.V,
        trajectories, opts.outFolder)
    }

    println("Producing transitions.")
    val positiveTransitions = TransitionProducer.positiveTransitions(
      directedStateGraph)
    val negativeTransitions = TransitionProducer.negativeTransitions(
      directedStateGraph, threeValuedExperiment.names)
    TransitionLogger.saveToFile(positiveTransitions,
      new File(opts.outFolder, "positive-transitions.csv"))
    TransitionLogger.saveToFile(negativeTransitions,
      new File(opts.outFolder, "negative-transitions.csv"))

    if (opts.runSynthesis) {
      println("Synthesizing.")
      val synthesizer = new Synthesizer(
        opts.synthOpts.maxExpressionDepth,
        opts.synthOpts.maxNbModels)
      val labelToSynthesisResults = synthesizer.synthesizeForAllLabels(
        positiveTransitions, negativeTransitions)

      if (opts.runSimulation) {
        println("Simulating functions.")
        val initialStates = StateGraphs.initialTrajectoryStates(
          directedStateGraph.V, trajectories)

        val observedStates = directedStateGraph.V.map(_.state)

        ReachabilityEvaluation.evaluate(labelToSynthesisResults,
          initialStates, observedStates, opts.outFolder)
      }
    }
  }

  private def readAndTransformContinuousExperiment(
    experimentFile: File,
    filterNames: Set[String],
    pseudoLogFactor: Option[Double]
  ): ContinuousExperiment = {
    println("Reading continuous experiment.")
    val e = ContinuousExperimentParser.parseAndFilter(experimentFile,
      if (filterNames.isEmpty) None else Some(filterNames))

    pseudoLogFactor match {
      case Some(factor) => {
        println("Transforming data.")
        ExperimentTransformation.pseudoLog(e, factor)
      }
      case None => e
    }
  }

  private def readClustering(
    fileOpt: Option[File]
  ): mutable.MultiMap[String, String] = fileOpt match {
    case Some(f) => ClusteringParser.parse(f)
    case None => new mutable.HashMap[String, mutable.Set[String]]
      with mutable.MultiMap[String, String]
  }
}
