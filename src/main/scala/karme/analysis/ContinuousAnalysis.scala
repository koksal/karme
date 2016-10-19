package karme.analysis

import karme.ContinuousExperiment

import scala.collection.mutable
import scala.io.Source

object ContinuousAnalysis {
  def analyze(
    exp: ContinuousExperiment,
    clustering: mutable.MultiMap[String, String]
  ): Unit = {
    val markers = Source.fromFile("data/markers.txt").getLines()

    // TODO variance for each marker within each cluster and globally
    // TODO box plots for lists of lists
  }

}
