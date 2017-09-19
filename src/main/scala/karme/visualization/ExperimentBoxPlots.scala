package karme.visualization

import java.io.File

import karme.Experiments.ContinuousExperiment

import scala.collection.mutable

object ExperimentBoxPlots {

  def plot(
    contExp: ContinuousExperiment,
    clustering: mutable.MultiMap[String, String],
    outFolder: File
  ): Unit = {
    val clusterToContExp = contExp.partitionClusters(clustering)
    for (name <- contExp.names) {
      val labelToValues = for (
        (cluster, contClusterExp) <- clusterToContExp) yield {
        val markerClusterValues = contClusterExp.valuesForName(name)
        cluster -> markerClusterValues
      }
      println(s"Plotting for $name")
      new BoxPlot().plot(labelToValues, name, outFolder)
    }
  }

}
