package karme.visualization

import java.io.File

import karme.Experiments.ContinuousExperiment

import scala.collection.mutable
import scala.io.Source

object ExperimentBoxPlots {

  def plot(
    contExp: ContinuousExperiment,
    clustering: mutable.MultiMap[String, String],
    outFolder: File
  ): Unit = {
    val markers = Source.fromFile("data/markers.txt").getLines()

    val clusterToContExp = contExp.partitionClusters(clustering)
    for (marker <- markers) {
      val labelToValues = for (
        (cluster, contClusterExp) <- clusterToContExp) yield {
        val markerClusterValues = contClusterExp.valuesForName(marker)
        cluster -> markerClusterValues
      }
      println(s"Plotting for $marker")
      BoxPlot.plot(labelToValues, marker, outFolder)
    }
  }

}