package karme.visualization

import java.io.File

import karme.Experiments.Experiment

import scala.reflect.ClassTag

class ExperimentHistogramPlotter {

  private val interface = new HistogramPlotInterface()

  def plotHistogramsPerVariable(
    e: Experiment[Double], folder: File
  ): Unit = {
    folder.mkdirs()

    for (name <- e.names) {
      val vs = e.valuesForName(name)
      val labels = vs map (v => name)
      val f = new File(folder, s"${name}.pdf")
      interface.plot(vs, labels, f)
    }
  }

  def plotLabeledHistograms[T: ClassTag, U: ClassTag](
    expToPlot: Experiment[T],
    expToLabel: Experiment[U],
    folder: File
  ): Unit = {
    require(expToPlot.measurements.map(_.id) ==
      expToLabel.measurements.map(_.id))

    folder.mkdirs()

    val namesToPlot = expToPlot.names

    for (name <- namesToPlot) {
      val values = expToPlot.valuesForName(name)
      val labels = if (expToLabel.names.contains(name)) {
        expToLabel.valuesForName(name)
      } else {
        values.map(_ => "none")
      }
      assert(values.size == labels.size)

      val f = new File(folder, s"$name.pdf")
      interface.plot(values, labels, f)
    }
  }

}
