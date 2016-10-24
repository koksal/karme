package karme.visualization

import java.io.File

import karme.Experiments.ContinuousExperiment
import karme.Experiments.DiscreteExperiment
import karme.util.FileUtil
import org.ddahl.rscala.RClient

import scala.collection.mutable

object DiscretizationHistogram {

  /** Plots histograms of different colors for every group of cells in which
    * a gene is discretized to the same value.
    */
  def visualizeDiscretization(
    contExp: ContinuousExperiment,
    discExp: DiscreteExperiment,
    clustering: mutable.MultiMap[String, String],
    outFolder: File
  ): Unit = {
    val clusterToContExp = contExp.partitionClusters(clustering)
    val clusterToDiscExp = discExp.partitionClusters(clustering)

    val clusters = clustering.keySet
    for (cluster <- clusters) {
      val clusterContExp = clusterToContExp(cluster)
      val clusterDiscExp = clusterToDiscExp(cluster)
      assert(clusterContExp.measurements.map(_.id) ==
        clusterDiscExp.measurements.map(_.id))

      visualizeDiscretization(clusterContExp, clusterDiscExp,
        new File(outFolder, s"cluster-$cluster"))
    }

    // visualize across clusters
    visualizeDiscretization(contExp, discExp, new File(outFolder, "all"))
  }

  private def visualizeDiscretization(
    contExp: ContinuousExperiment,
    discExp: DiscreteExperiment,
    outFolder: File
  ): Unit = {
    assert(contExp.names == discExp.names)

    val R = RClient()
    R.eval("library(ggplot2)")

    val contValuesPerName = contExp.measurements.map(_.values).transpose
    val discValuesPerName = discExp.measurements.map(_.values).transpose

    assert(contValuesPerName.size == discValuesPerName.size)

    for (((contValues, discValues), i) <-
         contValuesPerName.zip(discValuesPerName).zipWithIndex) {
      val name = contExp.names(i)

      R.set("contValues", contValues.toArray)
      R.set("discValues", discValues.map("Level " + _).toArray)
      R.eval("data <- data.frame(continuous = contValues, discrete = " +
        "discValues)")

      R.eval("plot = ggplot(data, aes(x=continuous, fill=discrete)) + " +
        "geom_histogram(alpha=.5, position=\"identity\")")

      val folderName = "discretization-vis"
      val folder = new File(outFolder, folderName)
      folder.mkdirs()
      val f = new File(folder, s"$name.pdf")

      R.set("plotFname", f.getAbsolutePath())
      R.eval("ggsave(plot, file = plotFname)")
    }
  }

}
