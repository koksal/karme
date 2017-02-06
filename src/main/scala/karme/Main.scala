package karme

import java.io.File

import karme.CellTrajectories.CellTrajectory
import karme.Experiments.ContinuousExperiment
import karme.Experiments.Experiment
import karme.clustering.HierarchicalClustering
import karme.transformations.BinomialMLE
import karme.discretization.Discretization
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.UndirectedStateGraphOps
import karme.parsing.BooleanExperimentParser
import karme.parsing.{CellTrajectoryParser, ClusteringParser, ContinuousExperimentParser}
import karme.printing.ExperimentLogger
import karme.printing.StatePseudotimeLogger
import karme.printing.TransitionLogger
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.Synthesis
import karme.transformations.ExperimentTransformation
import karme.transformations.TransitionProducer
import karme.visualization.{CurvePlot, DiscretizationHistogram, ExperimentBoxPlots, StateGraphVisualization}

import scala.collection.mutable
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    opts.outFolder.mkdirs()

    val continuousExperiment = readContinuousExperiment(
      opts.continuousExperimentFile.getOrElse(sys.error(
        "No continuous experiment given.")), opts.namesFiles)

    val annotationVars = opts.annotationsFile match {
      case Some(f) => readNames(f)
      case None => Set[String]()
    }

    val booleanExperiment = opts.discretizedExperimentFile match {
      case Some(f) => {
        BooleanExperimentParser.parse(f, None)
      }
      case None => {
        // first discretization pass via Ckmeans
        val binarized = Discretization.binarize(continuousExperiment)

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

    val trajectories = opts.trajectoryFiles map CellTrajectoryParser.parse

    val mleExperiment = opts.mleExperimentFile match {
      case Some(f) => {
        ContinuousExperimentParser.parse(f, None)
      }
      case None => {
        val res = BinomialMLE.run(booleanExperiment, trajectories,
          opts.analysisOptions.windowRadius)
        ExperimentLogger.saveToFile(res,
          new File(opts.outFolder, "experiment-mle.csv"))
        res
      }
    }
    plotExperiment(mleExperiment, trajectories, "mle", opts.outFolder)

    val clusteredExperiment = opts.analysisOptions.nbClusters match {
      case Some(nbClusters) => {
        println("Clustering variables.")
        val clusteredExp = HierarchicalClustering.clusteredExperiment(
          mleExperiment, nbClusters, opts.runElbow, opts.outFolder)
        ExperimentLogger.saveToFile(clusteredExp,
          new File(opts.outFolder, "experiment-clustered.csv"))
        plotExperiment(clusteredExp, trajectories, "mle-clustered",
          opts.outFolder)
        clusteredExp
      }
      case None => {
        mleExperiment
      }
    }

    println("Converting to three-valued states")
    val threeValuedExperiment =
      Experiments.probabilisticExperimentToThreeValued(clusteredExperiment)
    ExperimentLogger.saveToFile(threeValuedExperiment,
      new File(opts.outFolder, "experiment-three-valued.csv"))

    val cellClustering = readClustering(opts.clusterFile)

    println("Building graphs.")
    val undirectedStateGraph = StateGraphs.fromThreeValuedExperiment(
      threeValuedExperiment, opts.analysisOptions.maxHammingDistance)
    val directedStateGraph = UndirectedStateGraphOps.orientByTrajectories(
      undirectedStateGraph, trajectories)
    println(s"Produced an undirected state graph with " +
      s"|V| = ${undirectedStateGraph.V.size} and " +
      s"|E| = ${undirectedStateGraph.E.size}")

    if (opts.visualizationOptions.stateGraphs) {
      val initialGraphStates = StateGraphs.initialTrajectoryStates(
        undirectedStateGraph.V, trajectories)
      val nodeToID = StateGraphs.makeNodeIDs(directedStateGraph.V)
      val highlightGroups = List(initialGraphStates)
      StateGraphVisualization.plotUndirectedGraph(undirectedStateGraph,
        cellClustering, highlightGroups, "original", opts.outFolder)
      StateGraphVisualization.plotDirectedGraph(directedStateGraph,
        cellClustering, highlightGroups, opts.outFolder)
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

    println("Synthesizing.")
    val labelToFunctionExpressions = Synthesis.synthesizeForAllLabels(
      positiveTransitions, negativeTransitions)

    println("Simulating functions.")
    val initialStates = StateGraphs.initialTrajectoryStates(
      directedStateGraph.V, trajectories)
    val simulationStates =
      AsyncBooleanNetworkSimulation.pickFunctionsAndSimulate(
        labelToFunctionExpressions, initialStates)

    val actualStates = directedStateGraph.V.map(_.state)
    val commonStates = actualStates intersect simulationStates
    val missedStates = actualStates -- simulationStates
    val unobservedStates = simulationStates -- actualStates

    println(s"Common states: ${commonStates.size}")
    println(s"Missed states: ${missedStates.size}")
    println(s"Unobserved states: ${unobservedStates.size}")

    // visualization
    val visOpts = opts.visualizationOptions
    if (visOpts.stateGraphs) {
      // TODO plot simulation after it runs
      val highlightGroups = List(initialStates, unobservedStates,
        simulationStates)

      val actualSimulatedUnionExp = Experiments.booleanStatesToExperiment(
        simulationStates ++ actualStates)
      val unionStateGraph = StateGraphs.fromBooleanExperiment(
        actualSimulatedUnionExp, opts.analysisOptions.maxHammingDistance)
      StateGraphVisualization.plotUndirectedGraph(unionStateGraph,
        highlightGroups, "simulated", opts.outFolder)
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
    namesFiles: Seq[File]
  ): ContinuousExperiment = {
    val namesToFilter = if (namesFiles.isEmpty) {
      None
    } else {
      Some(namesFiles.map(
        nf => readNames(nf)).reduce(_.intersect(_)))
    }

    println("Reading continuous experiment.")
    val e = ContinuousExperimentParser.parse(experimentFile, namesToFilter)

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

  private def readNames(f: File): Set[String] = {
    val names = Source.fromFile(f).getLines().toSeq
    names.toSet
  }
}
