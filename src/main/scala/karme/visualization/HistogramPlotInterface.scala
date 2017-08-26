package karme.visualization

import java.io.File

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

import scala.reflect.ClassTag

class HistogramPlotInterface extends AbstractRInterface {

  override def LIBRARIES: Seq[String] = Seq("ggplot2")

  def plot[T: ClassTag](
    values: Seq[T],
    f: File
  ): Unit = {
    val labels = values map (v => "Unlabeled")
    plot(values, labels, f)
  }

  def plot[T: ClassTag, U: ClassTag](
    values: Seq[T],
    labels: Seq[U],
    f: File
  ): Unit = {
    assert(values.size == labels.size)

    if (values.isEmpty) {
      return
    }

    R.set("values", values.toArray)
    R.set("labels", labels.toArray.map(_.toString))

    R.eval("data <- data.frame(value = values, label = labels)")

    R.eval("plot = ggplot(data, aes(x=value, fill=label)) + " +
      "geom_histogram(alpha=.5, position=\"identity\")")

    R.set("plotFilename", f.getAbsolutePath())
    R.eval("ggsave(plot, file = plotFilename)")
  }

}
