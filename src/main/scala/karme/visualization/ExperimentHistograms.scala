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
      val labels = vs map (v => "none")
      val f = new File(histogramsFolder, s"${name}.pdf")
      new HistogramPlotInterface(vs, labels, f).run()
    }
  }

  def visualizeDiscretization[T: ClassTag, U: ClassTag](
    contExp: Experiment[T],
    discExp: Experiment[U],
    folder: File
  ): Unit = {
    assert(contExp.names == discExp.names)

    folder.mkdirs()

    val contValuesPerName = contExp.names.map(n => contExp.valuesForName(n))
    val discValuesPerName = discExp.names.map(n => discExp.valuesForName(n))

    assert(contValuesPerName.size == discValuesPerName.size)

    for (((contValues, discValues), i) <-
         contValuesPerName.zip(discValuesPerName).zipWithIndex) {
      val name = contExp.names(i)
      val f = new File(folder, s"$name.pdf")

      new HistogramPlotInterface(contValues, discValues, f).run()
    }
  }

}
