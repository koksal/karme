package karme

import java.io.File

import karme.discretization.Discretization
import karme.parsing.ContinuousExperimentParser
import karme.visualization.ExperimentVisualization

object Main {

  def main(args: Array[String]): Unit = {
    // read data
    println("Reading data.")
    val inputFile = new File(args(0))
    var experiment = ContinuousExperimentParser.parse(inputFile)

    // TODO
    // read / write discretization info
    // analyze discretized data with:
    //   optional clustering info

    // transform data
    println("Transforming data.")
    experiment = Transformations.pseudoLog(experiment)

    // discretize
    println("Discretizing data.")
    val discreteExperiment = Discretization.discretize(experiment)

    println("Visualizing discretization.")
    ExperimentVisualization.visualizeDiscretization(experiment,
      discreteExperiment)
  }

}
