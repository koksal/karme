package karme.visualization

import java.io.File

import karme.Experiments.BooleanExperiment
import karme.Experiments.ContinuousExperiment
import org.ddahl.rscala.RClient

import scala.collection.mutable

object DiscretizationHistogram {

  /** Plots histograms of different colors for every group of cells in which
    * a gene is discretized to the same value.
    */
  def visualizeDiscretization(
    contExp: ContinuousExperiment,
    discExp: BooleanExperiment,
    clustering: mutable.MultiMap[String, String],
    outFolder: File
  ): Unit = {
    val histogramsFolder = new File(outFolder, "histograms")
    histogramsFolder.mkdirs()

    val clusterToContExp = contExp.partitionClusters(clustering)
    val clusterToDiscExp = discExp.partitionClusters(clustering)

    val clusters = clustering.keySet
    for (cluster <- clusters) {
      val clusterContExp = clusterToContExp(cluster)
      val clusterDiscExp = clusterToDiscExp(cluster)
      assert(clusterContExp.measurements.map(_.id) ==
        clusterDiscExp.measurements.map(_.id))

      visualizeDiscretization(clusterContExp, clusterDiscExp,
        new File(histogramsFolder, s"cluster-$cluster"))
    }

    // visualize across clusters
    visualizeDiscretization(contExp, discExp, new File(histogramsFolder, "all"))
  }

  private def visualizeDiscretization(
    contExp: ContinuousExperiment,
    discExp: BooleanExperiment,
    folder: File
  ): Unit = {
    assert(contExp.names == discExp.names)

    val contValuesPerName = contExp.names.map(n => contExp.valuesForName(n))
    val discValuesPerName = discExp.names.map(n => discExp.valuesForName(n))

    assert(contValuesPerName.size == discValuesPerName.size)

    for (((contValues, discValues), i) <-
         contValuesPerName.zip(discValuesPerName).zipWithIndex) {
      val name = contExp.names(i)
      val f = new File(folder, s"$name.pdf")

      Histogram.plot(contValues, discValues, f)
    }
  }

}
