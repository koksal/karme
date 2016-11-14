package karme

import java.io.File

import karme.Experiments.{ContinuousExperiment, DiscreteExperiment, Experiment}
import karme.analysis.{BinomialMLE, ContinuousAnalysis, DiscreteStateAnalysis}
import karme.discretization.Discretization
import karme.parsing.{CellTrajectoryParser, ClusteringParser, ContinuousExperimentParser, DiscreteExperimentParser}
import karme.printing.ExperimentPrinter
import karme.visualization.CurvePlot
import karme.visualization.DiscreteStateGraphVisualization
import karme.visualization.DiscretizationHistogram

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

    saveExperiment(discreteExperiment, opts.outFolder)

    discreteExperiment = Transformations.removeNamesWithOneLevel(
      discreteExperiment)

    val trajectories = opts.trajectoryFiles map CellTrajectoryParser.parse
    println(s"Read ${trajectories.size} trajectories.")

    val probExperiment = BinomialMLE.run(discreteExperiment, trajectories,
      opts.analysisOptions.windowRadius)
    val discreteMLEExperiment = Experiments.discretizeProbabilisticExperiment(
      probExperiment)

    val clustering = readClustering(opts.clusterFile)

    if (opts.visualize) {
      continuousExperimentOpt match {
        case Some(e) => {
          println("Visualizing discretization.")
          DiscretizationHistogram.visualizeDiscretization(e,
            discreteExperiment, clustering, opts.outFolder)
        }
        case None => {
          println("No experiment to visualize.")
        }
      }
    }

    if (opts.continuousAnalysis) {
      ContinuousAnalysis.analyze(continuousExperimentOpt.get, clustering, opts.outFolder)
    }

    DiscreteStateGraphVisualization.plot(discreteMLEExperiment, clustering,
      opts.outFolder)

    val curveFolder = new File(opts.outFolder, "curves")
    for ((t, i) <- trajectories.zipWithIndex) {
      CurvePlot.plot(continuousExperimentOpt.get, t, new File(curveFolder,
        s"curve-$i-raw"))
      CurvePlot.plot(discreteExperiment, t, new File(curveFolder,
        s"curve-$i-raw-discrete"))
      CurvePlot.plot(probExperiment, t, new File(curveFolder,
        s"curve-$i-mle"))
      CurvePlot.plot(discreteMLEExperiment, t, new File(curveFolder,
        s"curve-$i-mle-discrete"))
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
      Transformations.pseudoLog(e)
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
    options: VisualizationOptions,
    outFolder: File
  ): Unit = {
    
  }

  private def saveExperiment[T](
    e: Experiment[T], outFolder: File
  ): Unit = {
    val fname = "discrete-experiment.csv"
    val f = new File(outFolder, fname)
    ExperimentPrinter.print(e, f)
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
      experiment.project(commonNames.toSeq.sorted)
    }
    case None => experiment
  }
}
