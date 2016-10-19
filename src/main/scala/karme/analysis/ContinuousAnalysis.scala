package karme.analysis

import karme.Experiments.ContinuousExperiment
import karme.visualization.BoxPlot

import scala.collection.mutable
import scala.io.Source

object ContinuousAnalysis {
  def analyze(
    exp: ContinuousExperiment,
    clustering: mutable.MultiMap[String, String]
  ): Unit = {
    val markers = Source.fromFile("data/markers.txt").getLines()

    val clusterToExperiment = exp.partitionClusters(clustering)
    for (marker <- markers) {
      val labelToValues = for (
        (cluster, clusterExp) <- clusterToExperiment) yield {
        val markerClusterValues = clusterExp.valuesForName(marker)
        cluster -> markerClusterValues
      }
      println(s"Plotting for $marker")
      BoxPlot.plot(labelToValues, marker, None)
    }
  }

}
