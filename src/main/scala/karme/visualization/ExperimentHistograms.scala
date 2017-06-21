package karme.visualization

import java.io.File

import karme.Experiments.Experiment

import scala.reflect.ClassTag

object ExperimentHistograms {

  def plotHistogramsPerVariable(
    e: Experiment[Double], outFolder: File
  ): Unit = {
    val histogramsFolder = new File(outFolder, "histograms")
    histogramsFolder.mkdirs()

    for (name <- e.names) {
      val vs = e.valuesForName(name)
      val labels = vs map (v => name)
      val f = new File(histogramsFolder, s"${name}.pdf")
      new HistogramPlotInterface(vs, labels, f).run()
    }
  }

  def plotLabeledHistograms[T: ClassTag, U: ClassTag](
    expToPlot: Experiment[T],
    expToLabel: Experiment[U],
    folder: File
  ): Unit = {
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
      new HistogramPlotInterface(values, labels, f).run()
    }
  }

}
