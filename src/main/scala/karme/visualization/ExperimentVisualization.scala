package karme.visualization

import java.io.File

import karme.Experiments.ContinuousExperiment
import karme.Experiments.DiscreteExperiment
import org.ddahl.rscala.RClient

object ExperimentVisualization {

  /** Plots histograms of different colors for every group of cells in which
    * a gene is discretized to the same value.
    */
  def visualizeDiscretization(
    contExp: ContinuousExperiment,
    discExp: DiscreteExperiment,
    outFolder: Option[File]
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
      val folder = outFolder match {
        case Some(of) => new File(of, folderName)
        case None => new File(folderName)
      }
      folder.mkdirs()
      val f = new File(folder, s"$name.pdf")

      R.set("plotFname", f.getAbsolutePath())
      R.eval("ggsave(plot, file = plotFname)")
    }
  }

}
