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
import karme.parsing.DiscreteExperimentParser
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

    val discreteExperiment = opts.discretizedExperimentFile match {
      case Some(f) => {
        DiscreteExperimentParser.parse(f, None)
      }
      case None => {
        val parsed = Discretization.discretize(continuousExperiment)
        var transformed = ExperimentTransformation.removeNamesWithOneLevel(
          parsed)
        transformed = ExperimentTransformation.removeMostlyInactiveVariables(
          transformed)
        ExperimentLogger.saveToFile(transformed,
          new File(opts.outFolder, "experiment-discretized-non-smoothed.csv"),
          None)
        transformed
      }
    }

    val trajectories = opts.trajectoryFiles map CellTrajectoryParser.parse
    println(s"Read ${trajectories.size} trajectories.")

    val mleExperiment = opts.mleExperimentFile match {
      case Some(f) => {
        println("Reading MLE from file.")
        ContinuousExperimentParser.parse(f, None)
      }
      case None => {
        println("Running MLE.")
        val res = BinomialMLE.run(discreteExperiment, trajectories,
          opts.analysisOptions.windowRadius)
        ExperimentLogger.saveToFile(res,
          new File(opts.outFolder, "experiment-mle.csv"), None)
        plotExperiment(res, trajectories, "mle", opts.outFolder)

        res
      }
    }

    // Read markers for some quick analysis
    val markers = readNames(new File("data/markers.txt"))

    println("Clustering variables.")
    val clusteredExp = HierarchicalClustering.clusteredExperiment(mleExperiment,
      opts.analysisOptions.nbClusters, opts.outFolder, markers)
    ExperimentLogger.saveToFile(clusteredExp,
      new File(opts.outFolder, "experiment-clustered.csv"), None)
    plotExperiment(clusteredExp, trajectories, "mle-clustered", opts.outFolder)

    println("Converting to three-valued states")
    val threeValuedExperiment =
      Experiments.probabilisticExperimentToThreeValued(clusteredExp)
    ExperimentLogger.saveToFile(threeValuedExperiment,
      new File(opts.outFolder, "experiment-three-valued.csv"), None)

    val clustering = readClustering(opts.clusterFile)

    println("Building graphs.")
    val undirectedStateGraph = StateGraphs.fromThreeValuedExperiment(
      threeValuedExperiment, opts.analysisOptions.maxHammingDistance)
    val directedStateGraph = UndirectedStateGraphOps.orientByTrajectories(
      undirectedStateGraph, trajectories)

    val initialGraphStates = StateGraphs.initialTrajectoryStates(
      undirectedStateGraph.V, trajectories)

    // plotting graphs
    val nodeToID = StateGraphs.makeNodeIDs(directedStateGraph.V)
    val highlightGroups = List(initialGraphStates)
    StateGraphVisualization.plotUndirectedGraph(undirectedStateGraph,
      clustering, nodeToID, highlightGroups, "original", opts.outFolder)
    StateGraphVisualization.plotDirectedGraph(directedStateGraph, clustering,
      nodeToID, highlightGroups, opts.outFolder)

    println("Producing transitions.")
    val positiveTransitions = TransitionProducer.positiveTransitions(
      directedStateGraph)
    val negativeTransitions = TransitionProducer.negativeTransitions(
      directedStateGraph, threeValuedExperiment.names)
    TransitionLogger.saveToFile(positiveTransitions,
      new File(opts.outFolder, "positive-transitions.csv"))
    TransitionLogger.saveToFile(negativeTransitions,
      new File(opts.outFolder, "negative-transitions.csv"))

    val cellToNodeID = StateGraphs.makeCellIDs(nodeToID)

    StatePseudotimeLogger.savePseudotimes(directedStateGraph.V, trajectories,
      nodeToID, opts.outFolder)

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

    // TODO print into file
    println(s"Common states: ${commonStates.size}")
    println(s"Missed states: ${missedStates.size}")
    println(s"Unobserved states: ${unobservedStates.size}")

    // visualization
    val visOpts = opts.visualizationOptions
    if (visOpts.histograms) {
      DiscretizationHistogram.visualizeDiscretization(
        continuousExperiment, discreteExperiment, clustering,
        opts.outFolder)
    }

    if (visOpts.boxPlots) {
      ExperimentBoxPlots.plot(continuousExperiment, clustering,
        opts.outFolder)
    }

    if (visOpts.stateGraph) {
      val highlightGroups = List(initialStates, unobservedStates,
        simulationStates)

      StateGraphVisualization.plotUndirectedGraph(undirectedStateGraph,
        clustering, nodeToID, highlightGroups, "original", opts.outFolder)
      StateGraphVisualization.plotDirectedGraph(directedStateGraph, clustering,
        nodeToID, highlightGroups, opts.outFolder)
      StateGraphVisualization.plotTransitions(directedStateGraph, clustering,
        positiveTransitions, nodeToID, opts.outFolder)

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
