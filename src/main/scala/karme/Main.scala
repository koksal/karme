package karme

import java.io.File

import karme.discretization.Discretization
import karme.parsing.ContinuousExperimentParser

object Main {

  def main(args: Array[String]): Unit = {
    // read data
    val inputFile = new File(args(0))
    var experiment = ContinuousExperimentParser.parse(inputFile)

    // transform data
    experiment = Transformations.pseudoLog(experiment)

    // discretize
    val discreteExperiment = Discretization.discretize(experiment)
  }

}
