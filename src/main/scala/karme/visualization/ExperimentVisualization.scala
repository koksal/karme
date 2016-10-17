package karme.visualization

import karme.{ContinuousExperiment, DiscreteExperiment}
import org.ddahl.rscala.RClient

object ExperimentVisualization {

  /** Plots histograms of different colors for every group of cells in which
    * a gene is discretized to the same value.
    */
  def visualizeDiscretization(contExp: ContinuousExperiment,
                              discExp: DiscreteExperiment): Unit = {
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

      R.set("plotFname", s"$name.pdf")
      R.eval("ggsave(plot, file = plotFname)")
    }
  }

}
