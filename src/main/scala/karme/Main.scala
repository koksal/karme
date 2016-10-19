package karme

import java.io.File

import karme.Experiments.ContinuousExperiment
import karme.Experiments.Experiment
import karme.analysis.ContinuousAnalysis
import karme.analysis.DiscreteStateAnalysis
import karme.discretization.Discretization
import karme.parsing.ClusteringParser
import karme.parsing.ContinuousExperimentParser
import karme.parsing.DiscreteExperimentParser
import karme.printing.ExperimentPrinter
import karme.visualization.ExperimentVisualization

import scala.collection.mutable

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)

    var experiment: Option[ContinuousExperiment] =
      opts.continuousExperimentFile map { f =>
        println("Reading data.")
        ContinuousExperimentParser.parse(f)
      }

    experiment = experiment map { e =>
      println("Transforming data.")
      Transformations.pseudoLog(e)
    }

    val discreteExperiment = opts.discreteExperimentFile match {
      case Some(f) =>
        println("Reading discretized experiment from file.")
        DiscreteExperimentParser.parse(f)
      case None =>
        if (opts.discretize) {
          println("Discretizing experiment.")
          experiment match {
            case Some(e) =>
              val de = Discretization.discretize(e)
              println("Saving to file.")
              saveExperiment(de, opts.outFolder)
              de
            case None => sys.error("No continuous experiment given.")
          }
        } else {
          null
        }
    }

    val clustering: mutable.MultiMap[String, String] = opts.clusterFile match {
      case Some(f) => ClusteringParser.parse(f)
      case None => new mutable.HashMap[String, mutable.Set[String]]
        with mutable.MultiMap[String, String]
    }

    if (opts.visualize) {
      experiment match {
        case Some(e) =>
          println("Visualizing discretization.")
          ExperimentVisualization.visualizeDiscretization(e,
            discreteExperiment, opts.outFolder)
        case None =>
      }
    }

    if (opts.analyzeDiscreteStates) {
      DiscreteStateAnalysis.analyze(discreteExperiment, clustering)
    }

    if (opts.analyzeContinuousStates) {
      ContinuousAnalysis.analyze(experiment.get, clustering)
    }
  }

  private def saveExperiment[T](
    e: Experiment[T], outFolder: Option[File]
  ): Unit = {
    val fname = "discrete-experiment.csv"
    val f = outFolder match {
      case Some(of) => of.mkdirs(); new File(of, fname)
      case None => new File(fname)
    }
    ExperimentPrinter.print(e, f)
  }
}
