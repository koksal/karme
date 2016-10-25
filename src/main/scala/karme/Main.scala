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
import karme.visualization.DiscreteStateGraph
import karme.visualization.DiscretizationHistogram

import scala.collection.mutable
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    opts.outFolder.mkdirs()

    var experiment: Option[ContinuousExperiment] =
      opts.continuousExperimentFile map { f =>
        println("Reading data.")
        ContinuousExperimentParser.parse(f)
      }

    experiment = experiment map { e =>
      println("Transforming data.")
      Transformations.pseudoLog(e)
    }

    opts.namesFile match {
      case Some(nf) => {
        val names = Source.fromFile(nf).getLines().toSeq
        experiment = experiment.map(_.project(names))
      }
      case None =>
    }

    val discreteExperiment = opts.discreteExperimentFile match {
      case Some(f) => {
        println("Reading discretized experiment from file.")
        var de = DiscreteExperimentParser.parse(f)
        opts.namesFile match {
          case Some(nf) => {
            val names = Source.fromFile(nf).getLines().toSeq
            de = de.project(names)
          }
          case None =>
        }
        de
      }
      case None => {
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
          DiscretizationHistogram.visualizeDiscretization(e,
            discreteExperiment, clustering, opts.outFolder)
        case None =>
      }
    }

    if (opts.discreteAnalysis) {
      DiscreteStateAnalysis.analyze(discreteExperiment, clustering)
    }

    if (opts.continuousAnalysis) {
      ContinuousAnalysis.analyze(experiment.get, clustering, opts.outFolder)
    }

    DiscreteStateGraph.plot(discreteExperiment, clustering, opts.outFolder)
  }

  private def saveExperiment[T](
    e: Experiment[T], outFolder: File
  ): Unit = {
    val fname = "discrete-experiment.csv"
    val f = new File(outFolder, fname)
    ExperimentPrinter.print(e, f)
  }
}
