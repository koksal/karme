package karme

import java.io.File

import karme.Experiments.{ContinuousExperiment, DiscreteExperiment, Experiment}
import karme.transformations.BinomialMLE
import karme.discretization.Discretization
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.UndirectedStateGraphOps
import karme.parsing.{CellTrajectoryParser, ClusteringParser, ContinuousExperimentParser, DiscreteExperimentParser}
import karme.printing.ExperimentLogger
import karme.printing.StatePseudotimeLogger
import karme.printing.TransitionLogger
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.Synthesis
import karme.transformations.ContinuousTransformations
import karme.transformations.TransitionProducer
import karme.visualization.{CurvePlot, DiscretizationHistogram, ExperimentBoxPlots, StateGraphVisualization}

import scala.collection.mutable
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    opts.outFolder.mkdirs()

    val continuousExperimentOpt = readContinuousExperiment(
      opts.continuousExperimentFile, opts.namesFiles)

    ExperimentLogger.saveToFile(continuousExperimentOpt.get,
      new File("filtered-continuous-experiment.csv"), None)
    sys.exit(0)

    var discreteExperiment = Discretization.discretize(
      continuousExperimentOpt.getOrElse(sys.error(
        "No continuous or discrete experiment given.")))

    discreteExperiment = ContinuousTransformations.removeNamesWithOneLevel(
      discreteExperiment)

    val trajectories = opts.trajectoryFiles map CellTrajectoryParser.parse
    println(s"Read ${trajectories.size} trajectories.")

    val mleExperiment = BinomialMLE.run(discreteExperiment, trajectories,
      opts.analysisOptions.windowRadius)

    val threeValuedExperiment =
      Experiments.probabilisticExperimentToThreeValued(mleExperiment)

    val clustering = readClustering(opts.clusterFile)

    val undirectedStateGraph = StateGraphs.fromThreeValuedExperiment(
      threeValuedExperiment, opts.analysisOptions.maxHammingDistance)
    val directedStateGraph = UndirectedStateGraphOps.orientByTrajectories(
      undirectedStateGraph, trajectories)

    val positiveTransitions = TransitionProducer.positiveTransitions(
      directedStateGraph)

    val negativeTransitions = TransitionProducer.negativeTransitions(
      directedStateGraph, threeValuedExperiment.names)

    val nodeToID = StateGraphs.makeNodeIDs(directedStateGraph.V)
    val cellToNodeID = StateGraphs.makeCellIDs(nodeToID)

    ExperimentLogger.saveToFile(discreteExperiment,
      new File(opts.outFolder, "experiment-first-discretization.csv"),
      Some(cellToNodeID) )
    ExperimentLogger.saveToFile(mleExperiment,
      new File(opts.outFolder, "experiment-mle.csv"), Some(cellToNodeID))
    ExperimentLogger.saveToFile(threeValuedExperiment,
      new File(opts.outFolder, "experiment-three-valued.csv"),
      Some(cellToNodeID))

    TransitionLogger.saveToFile(positiveTransitions,
      new File(opts.outFolder, "positive-transitions.csv"))
    TransitionLogger.saveToFile(negativeTransitions,
      new File(opts.outFolder, "negative-transitions.csv"))

    StatePseudotimeLogger.savePseudotimes(directedStateGraph.V, trajectories,
      nodeToID, opts.outFolder)

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
        continuousExperimentOpt.get, discreteExperiment, clustering,
        opts.outFolder)
    }

    if (visOpts.boxPlots) {
      ExperimentBoxPlots.plot(continuousExperimentOpt.get, clustering,
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
      val unionStateGraph = StateGraphs.fromDiscreteExperiment(
        actualSimulatedUnionExp, opts.analysisOptions.maxHammingDistance)
      StateGraphVisualization.plotUndirectedGraph(unionStateGraph,
        highlightGroups, "simulated", opts.outFolder)
    }

    if (visOpts.curves) {
      val curveFolder = new File(opts.outFolder, "curves")
      for ((t, i) <- trajectories.zipWithIndex) {
        CurvePlot.plot(continuousExperimentOpt.get, t, new File(curveFolder,
          s"curve-$i-continuous"))
        CurvePlot.plot(discreteExperiment, t, new File(curveFolder,
          s"curve-$i-discrete"))
      }
    }

  }

  private def readContinuousExperiment(
    experimentFile: Option[File],
    namesFiles: Seq[File]
  ): Option[ContinuousExperiment] = {
    experimentFile map { f =>
      val namesToFilter = if (namesFiles.isEmpty) {
        None
      } else {
        Some(namesFiles.map(
          f => readNamesToFilter(f)).reduce(_.intersect(_)))
      }

      println("Reading continuous experiment.")
      val e = ContinuousExperimentParser.parse(f, namesToFilter)

      println("Transforming data.")
      ContinuousTransformations.pseudoLog(e)
    }
  }

  private def readClustering(
    fileOpt: Option[File]
  ): mutable.MultiMap[String, String] = fileOpt match {
    case Some(f) => ClusteringParser.parse(f)
    case None => new mutable.HashMap[String, mutable.Set[String]]
      with mutable.MultiMap[String, String]
  }

  private def readNamesToFilter(f: File): Set[String] = {
    val names = Source.fromFile(f).getLines().toSeq
    names.toSet
  }
}
