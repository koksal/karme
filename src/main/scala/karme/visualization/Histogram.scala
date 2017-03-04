package karme.visualization

import java.io.File

import org.ddahl.rscala.RClient

import scala.reflect.ClassTag

object Histogram {

  def plot[T: ClassTag, U: ClassTag](
    values: Seq[T],
    fillValues: Seq[U],
    f: File
  ): Unit = {
    assert(values.size == fillValues.size)

    val R = RClient()
    R.eval("library(ggplot2)")

    R.set("values", values.toArray)
    R.set("fillValues", fillValues.toArray)

    R.eval("data <- data.frame(value = values, fillValue = fillValues)")

    R.eval("plot = ggplot(data, aes(x=value, fill=fillValue)) + " +
      "geom_histogram(alpha=.5, position=\"identity\")")

    R.set("plotFilename", f.getAbsolutePath())
    R.eval("ggsave(plot, file = plotFilename)")
  }

}
