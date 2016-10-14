package karme

import java.io.File

import karme.parsing.ContinuousExperimentParser

object Main {

  val ARCSINH_FACTOR = 2.0

  def main(args: Array[String]): Unit = {
    // read data
    val inputFile = new File(args(0))
    var experiment = ContinuousExperimentParser.parse(inputFile)

    // transform data
    experiment = Transformations.pseudoLog(experiment, ARCSINH_FACTOR)

    // discretize
  }
}
