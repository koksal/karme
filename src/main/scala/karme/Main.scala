package karme

import java.io.File

import karme.Experiments.ContinuousExperiment
import karme.Experiments.Experiment
import karme.analysis.{BinomialMLE, ContinuousAnalysis, DiscreteStateAnalysis}
import karme.discretization.Discretization
import karme.parsing.{CellTrajectoryParser, ClusteringParser, ContinuousExperimentParser, DiscreteExperimentParser}
import karme.printing.ExperimentPrinter
import karme.visualization.DiscreteStateGraphVisualization
import karme.visualization.DiscretizationHistogram

import scala.collection.mutable
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    opts.outFolder.mkdirs()

    val experiment: Option[ContinuousExperiment] =
      opts.continuousExperimentFile map { f =>
        println("Reading continuous experiment.")
        var e = ContinuousExperimentParser.parse(f)

        println("Filtering by names.")
        e = filterByNames(e, opts.namesFile)

        println("Transforming data.")
        Transformations.pseudoLog(e)
      }

    var discreteExperiment = opts.discreteExperimentFile match {
      case Some(f) => {
        println("Reading discretized experiment.")
        val de = DiscreteExperimentParser.parse(f)

        println("Filtering by names.")
        filterByNames(de, opts.namesFile)
      }
      case None => {
        println("Discretizing experiment.")
        experiment match {
          case Some(e) => {
            val de = Discretization.discretize(e)

            println("Saving discrete experiment to file.")
            saveExperiment(de, opts.outFolder)

            de
          }
          case None => sys.error("No continuous experiment given.")
        }
      }
    }

    println("Removing dimensions with a single discrete value.")
    discreteExperiment =
      Transformations.removeNamesWithOneLevel(discreteExperiment)
    println(s"Remaining number of dimensions: ${discreteExperiment.names.size}")

    val trajectories = opts.trajectoryFiles map CellTrajectoryParser.parse
    println(s"Read ${trajectories.size} trajectories.")

    val probExperiment = BinomialMLE.run(discreteExperiment, trajectories,
      opts.analysisOptions.windowRadius)
    val discreteMLEExperiment =
      Experiments.discretizeProbabilisticExperiment(probExperiment)

    val clustering: mutable.MultiMap[String, String] = opts.clusterFile match {
      case Some(f) => ClusteringParser.parse(f)
      case None => new mutable.HashMap[String, mutable.Set[String]]
        with mutable.MultiMap[String, String]
    }

    if (opts.visualize) {
      experiment match {
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

    if (opts.discreteAnalysis) {
      DiscreteStateAnalysis.analyze(discreteExperiment, clustering)
    }

    if (opts.continuousAnalysis) {
      ContinuousAnalysis.analyze(experiment.get, clustering, opts.outFolder)
    }

    DiscreteStateGraphVisualization.plot(discreteMLEExperiment, clustering, opts.outFolder)
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
