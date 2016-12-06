package karme

import java.io.File

import karme.CellTrajectories.CellTrajectory
import karme.Experiments.{ContinuousExperiment, DiscreteExperiment, Experiment}
import karme.transformations.BinomialMLE
import karme.discretization.Discretization
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.UndirectedStateGraphOps
import karme.graphs.StateGraphs.{DirectedStateGraph, UndirectedStateGraph}
import karme.parsing.{CellTrajectoryParser, ClusteringParser, ContinuousExperimentParser, DiscreteExperimentParser}
import karme.printing.ExperimentLogger
import karme.printing.TransitionLogger
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
      Experiments.discretizeProbabilisticExperiment( mleExperiment)

    ExperimentLogger.saveToFile(discreteExperiment,
      new File(opts.outFolder, "experiment-first-discretization.csv"))
    ExperimentLogger.saveToFile(mleExperiment,
      new File(opts.outFolder, "experiment-mle.csv"))
    ExperimentLogger.saveToFile(thresholdedMLEExperiment,
      new File(opts.outFolder, "experiment-mle-thresholded.csv"))

    val clustering = readClustering(opts.clusterFile)

    val undirectedStateGraph = StateGraphs.fromDiscreteExperiment(
      thresholdedMLEExperiment, opts.analysisOptions.maxHammingDistance)
    val directedStateGraph = UndirectedStateGraphOps.orientByTrajectories(
      undirectedStateGraph, trajectories)

    val positiveTransitions = TransitionProducer.positiveTransitions(
      directedStateGraph, mleExperiment)

    TransitionLogger.saveToFile(positiveTransitions,
      new File(opts.outFolder, "transitions.csv"))

    visualize(continuousExperimentOpt.get, thresholdedMLEExperiment, clustering,
      trajectories, undirectedStateGraph, directedStateGraph,
      positiveTransitions, opts.visualizationOptions, opts.outFolder)
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
    undirectedStateGraph: UndirectedStateGraph,
    directedStateGraph: DirectedStateGraph,
    transitions: Iterable[Transition],
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
        clustering, outFolder)
      StateGraphVisualization.plotDirectedGraph(directedStateGraph, clustering,
        outFolder)
      StateGraphVisualization.plotTransitions(directedStateGraph, clustering,
        transitions, outFolder)
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
}
