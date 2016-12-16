package karme

import java.io.File

import karme.CellTrajectories.CellTrajectory
import karme.Experiments.{ContinuousExperiment, DiscreteExperiment, Experiment}
import karme.transformations.BinomialMLE
import karme.discretization.Discretization
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.StateGraphVertex
import karme.graphs.StateGraphs.UndirectedStateGraphOps
import karme.graphs.StateGraphs.{DirectedBooleanStateGraph, UndirectedBooleanStateGraph}
import karme.parsing.{CellTrajectoryParser, ClusteringParser, ContinuousExperimentParser, DiscreteExperimentParser}
import karme.printing.ExperimentLogger
import karme.printing.TransitionLogger
import karme.synthesis.Synthesis
import karme.synthesis.Transitions.Transition
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

    val thresholdedMLEExperiment =
      Experiments.discretizeProbabilisticExperiment(mleExperiment)

    val threeValuedExperiment =
      Experiments.probabilisticExperimentToThreeValued(mleExperiment)

    val clustering = readClustering(opts.clusterFile)

    val undirectedStateGraph = StateGraphs.fromThreeValuedExperiment(
      threeValuedExperiment, opts.analysisOptions.maxHammingDistance)
    val directedStateGraph = UndirectedStateGraphOps.orientByTrajectories(
      undirectedStateGraph, trajectories)

    val positiveTransitions = TransitionProducer.positiveTransitions(
      directedStateGraph, mleExperiment)

    val negativeTransitions = TransitionProducer.negativeTransitions(
      directedStateGraph, mleExperiment)

    val nodeToID = makeNodeIDs(directedStateGraph.V)
    val cellToNodeID = makeCellIDs(nodeToID)

    ExperimentLogger.saveToFile(discreteExperiment, cellToNodeID,
      new File(opts.outFolder, "experiment-first-discretization.csv"))
    ExperimentLogger.saveToFile(mleExperiment, cellToNodeID,
      new File(opts.outFolder, "experiment-mle.csv"))
    ExperimentLogger.saveToFile(thresholdedMLEExperiment, cellToNodeID,
      new File(opts.outFolder, "experiment-mle-thresholded.csv"))
    ExperimentLogger.saveToFile(threeValuedExperiment, cellToNodeID,
      new File(opts.outFolder, "experiment-three-valued.csv"))

    TransitionLogger.saveToFile(positiveTransitions,
      new File(opts.outFolder, "positive-transitions.csv"))
    TransitionLogger.saveToFile(negativeTransitions,
      new File(opts.outFolder, "negative-transitions.csv"))

    visualize(continuousExperimentOpt.get, thresholdedMLEExperiment, clustering,
      trajectories, undirectedStateGraph, directedStateGraph,
      positiveTransitions, nodeToID, opts.visualizationOptions, opts.outFolder)

    val labelToFunctionExpressions = Synthesis.synthesizeForAllLabels(
      positiveTransitions, negativeTransitions)

    // TODO factor out code that computes average state pseudotimes and pick
    // state that's earliest.
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

  private def visualize(
    continuousExperiment: ContinuousExperiment,
    discreteExperiment: DiscreteExperiment,
    clustering: mutable.MultiMap[String, String],
    trajectories: Seq[CellTrajectory],
    undirectedStateGraph: UndirectedBooleanStateGraph,
    directedStateGraph: DirectedBooleanStateGraph,
    transitions: Iterable[Transition],
    nodeToID: Map[StateGraphVertex, String],
    options: VisualizationOptions,
    outFolder: File
  ): Unit = {
    if (options.histograms) {
      DiscretizationHistogram.visualizeDiscretization(continuousExperiment,
        discreteExperiment, clustering, outFolder)
    }

    if (options.boxPlots) {
      ExperimentBoxPlots.plot(continuousExperiment, clustering, outFolder)
    }

    if (options.stateGraph) {
      StateGraphVisualization.plotUndirectedGraph(undirectedStateGraph,
        clustering, nodeToID, outFolder)
      StateGraphVisualization.plotDirectedGraph(directedStateGraph, clustering,
        nodeToID, outFolder)
      StateGraphVisualization.plotTransitions(directedStateGraph, clustering,
        transitions, nodeToID, outFolder)
    }

    if (options.curves) {
      val curveFolder = new File(outFolder, "curves")
      for ((t, i) <- trajectories.zipWithIndex) {
        CurvePlot.plot(continuousExperiment, t, new File(curveFolder,
          s"curve-$i-continuous"))
        CurvePlot.plot(discreteExperiment, t, new File(curveFolder,
          s"curve-$i-discrete"))
      }
    }
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

  def makeNodeIDs(
    vs: Iterable[StateGraphVertex]
  ): Map[StateGraphVertex, String] = {
    vs.toSeq.sorted.zipWithIndex.map{
      case (v, i) => {
        v -> s"V$i"
      }
    }.toMap
  }

  def makeCellIDs(
    nodeToID: Map[StateGraphVertex, String]
  ): Map[String, String] = {
    val cellIDs = nodeToID flatMap {
      case (node, id) => node.measurements.map(m => m.id -> id)
    }

    cellIDs.toMap
  }

}
