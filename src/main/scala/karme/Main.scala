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
      opts.continuousExperimentFile, opts.namesFile)

    var discreteExperiment = readDiscreteExperiment(
      opts.discreteExperimentFile, opts.namesFile).getOrElse(
        Discretization.discretize(continuousExperimentOpt.getOrElse(
          sys.error("No continuous or discrete experiment given."))))

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

    ExperimentLogger.saveToFile(discreteExperiment, cellToNodeID,
      new File(opts.outFolder, "experiment-first-discretization.csv"))
    ExperimentLogger.saveToFile(mleExperiment, cellToNodeID,
      new File(opts.outFolder, "experiment-mle.csv"))
    ExperimentLogger.saveToFile(threeValuedExperiment, cellToNodeID,
      new File(opts.outFolder, "experiment-three-valued.csv"))

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
      val highlightGroups = List(initialStates, simulationStates)

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
    namesFile: Option[File]
  ): Option[ContinuousExperiment] = {
    experimentFile map { f =>
      println("Reading continuous experiment.")
      var e = ContinuousExperimentParser.parse(f)

      println("Filtering by names.")
      e = filterByNames(e, namesFile)

      println("Transforming data.")
      ContinuousTransformations.pseudoLog(e)
    }
  }

  private def readDiscreteExperiment(
    experimentFile: Option[File],
    namesFile: Option[File]
  ): Option[DiscreteExperiment] = {
    experimentFile map { f =>
      println("Reading discretized experiment.")
      val de = DiscreteExperimentParser.parse(f)

      println("Filtering by names.")
      filterByNames(de, namesFile)
    }
  }

  private def readClustering(
    fileOpt: Option[File]
  ): mutable.MultiMap[String, String] = fileOpt match {
    case Some(f) => ClusteringParser.parse(f)
    case None => new mutable.HashMap[String, mutable.Set[String]]
      with mutable.MultiMap[String, String]
  }

  private def filterByNames[T](
    experiment: Experiment[T], optNamesFile: Option[File]
  ): Experiment[T] = optNamesFile match {
    case Some(nf) => {
      val names = Source.fromFile(nf).getLines().toSeq
      println(s"Filtering down to ${names.size} names.")
      val commonNames = experiment.names filter { n1 =>
        names.exists((n2: String) => n2.toUpperCase().equals(n1.toUpperCase()))
      }

      println(s"Names in common with experiment: ${commonNames.size}")
      experiment.project(commonNames.sorted)
    }
    case None => experiment
  }
}
