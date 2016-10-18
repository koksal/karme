package karme

import java.io.File

import karme.discretization.Discretization
import karme.parsing.ContinuousExperimentParser
import karme.parsing.DiscreteExperimentParser
import karme.printing.ExperimentPrinter
import karme.visualization.ExperimentVisualization

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
        println("Discretizing experiment.")
        experiment match {
          case Some(e) =>
            val de = Discretization.discretize(e)
            println("Saving to file.")
            saveExperiment(de, opts.outFolder)
            de
          case None => sys.error("No discrete or continuous experiment given.")
        }
    }

    experiment match {
      case Some(e) =>
        println("Visualizing discretization.")
        ExperimentVisualization.visualizeDiscretization(e,
          discreteExperiment, opts.outFolder)
      case None =>
    }
  }

  private def saveExperiment[MT <: Measurement[_]](
    e: Experiment[_,MT], outFolder: Option[File]
  ): Unit = {
    val fname = "discrete-experiment.csv"
    val f = outFolder match {
      case Some(of) => of.mkdirs(); new File(of, fname)
      case None => new File(fname)
    }
    ExperimentPrinter.print(e, f)
  }
}
