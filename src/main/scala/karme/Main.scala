package karme

import java.io.File

import karme.CellTrajectories.CellTrajectory
import karme.Experiments.ContinuousExperiment
import karme.Experiments.Experiment
import karme.clustering.HierarchicalClustering
import karme.transformations.BinomialMLE
import karme.discretization.Discretization
import karme.evaluation.ReachabilityEvaluation
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.UndirectedStateGraphOps
import karme.parsing.BooleanExperimentParser
import karme.parsing.ExperimentParser
import karme.parsing.NamesParser
import karme.parsing.{CellTrajectoryParser, ClusteringParser, ContinuousExperimentParser}
import karme.printing.ExperimentLogger
import karme.printing.StatePseudotimeLogger
import karme.printing.TransitionLogger
import karme.synthesis.Synthesis
import karme.transformations.ExperimentTransformation
import karme.transformations.TransitionProducer
import karme.visualization.{CurvePlot, StateGraphVisualization}

import scala.collection.mutable

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    opts.outFolder.mkdirs()

    val filterVars = NamesParser(opts.namesFiles)

    val continuousExperiment = readContinuousExperiment(
      opts.continuousExperimentFile.getOrElse(sys.error(
        "No continuous experiment given.")), filterVars)

    val annotationVars = ExperimentParser.selectNames(
      continuousExperiment.names.toSet, NamesParser(opts.annotationsFiles))

    val booleanExperiment = opts.discretizedExperimentFile match {
      case Some(f) => {
        BooleanExperimentParser.parse(f, None)
      }
      case None => {
        println("Discretizing.")

        // first discretization pass via Ckmeans
        val binarized = Discretization.binarize(continuousExperiment)

        println("Filtering.")
        // compute set of variables to filter out
        val variablesWithOneLevel = ExperimentTransformation.namesWithOneLevel(
          binarized)
        val inactiveVars = ExperimentTransformation.inactiveVariables(
          binarized, opts.analysisOptions.cellActivityThreshold)
        val varsToDrop = variablesWithOneLevel ++ inactiveVars -- annotationVars
        val filtered = binarized.project(binarized.names.toSet -- varsToDrop)

        ExperimentLogger.saveToFile(filtered,
          new File(opts.outFolder, "experiment-discretized-filtered.csv"))

        filtered
      }
    }

    println("Reading trajectories.")
    val trajectories = opts.trajectoryFiles map CellTrajectoryParser.parse

    val mleExperiment = opts.mleExperimentFile match {
      case Some(f) => {
        ContinuousExperimentParser.parse(f, None)
      }
      case None => {
        println("Computing MLE.")

        val res = BinomialMLE.run(booleanExperiment, trajectories,
          opts.analysisOptions.windowRadius)
        ExperimentLogger.saveToFile(res,
          new File(opts.outFolder, "experiment-mle.csv"))
        res
      }
    }
    if (opts.visualizationOptions.curves) {
      plotExperiment(mleExperiment, trajectories, "mle", opts.outFolder)
    }

    val clusteredExperiment = opts.analysisOptions.nbClusters match {
      case Some(nbClusters) => {
        println("Clustering variables.")
        val clusteredExp = HierarchicalClustering.clusteredExperiment(
          mleExperiment, nbClusters, annotationVars, opts.runElbow,
          opts.outFolder)
        ExperimentLogger.saveToFile(clusteredExp,
          new File(opts.outFolder, "experiment-clustered.csv"))
        if (opts.visualizationOptions.curves) {
          plotExperiment(clusteredExp, trajectories, "mle-clustered",
            opts.outFolder)
        }
        clusteredExp
      }
      case None => {
        mleExperiment
      }
    }

    println("Converting to three-valued states")
    val threeValuedExperiment =
      Experiments.probabilisticExperimentToThreeValued(clusteredExperiment,
        opts.analysisOptions.uncertaintyMargin)
    ExperimentLogger.saveToFile(threeValuedExperiment,
      new File(opts.outFolder, "experiment-three-valued.csv"))

    val cellClustering = readClustering(opts.clusterFile)

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
      val labelToSynthesisResults = Synthesis.synthesizeForAllLabels(
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

  private def plotExperiment[T](
    exp: Experiment[T],
    trajectories: Seq[CellTrajectory],
    name: String,
    outFolder: File
  ): Unit = {
    val curveFolder = new File(outFolder, "curves")
    for ((t, i) <- trajectories.zipWithIndex) {
      CurvePlot.plot(exp, t, new File(curveFolder, s"curve-$i-$name"))
    }
  }

  private def readContinuousExperiment(
    experimentFile: File,
    filterNames: Set[String]
  ): ContinuousExperiment = {
    println("Reading continuous experiment.")
    val e = ContinuousExperimentParser.parse(experimentFile,
      if (filterNames.isEmpty) None else Some(filterNames))

    println("Transforming data.")
    ExperimentTransformation.pseudoLog(e)
  }

  private def readClustering(
    fileOpt: Option[File]
  ): mutable.MultiMap[String, String] = fileOpt match {
    case Some(f) => ClusteringParser.parse(f)
    case None => new mutable.HashMap[String, mutable.Set[String]]
      with mutable.MultiMap[String, String]
  }
}
