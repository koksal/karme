package karme.visualization

import java.io.File

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

import scala.reflect.ClassTag

class HistogramPlotInterface[T: ClassTag, U: ClassTag](
  values: Seq[T],
  labels: Seq[U],
  f: File
) extends AbstractRInterface[Unit] {

  override val LIBRARIES: Seq[String] = Seq("ggplot2")

  def process(R: RClient): Unit = {
    assert(values.size == labels.size)

    R.set("values", values.toArray)
    R.set("labels", labels.toArray)

    R.eval("data <- data.frame(value = values, label = labels)")

    R.eval("plot = ggplot(data, aes(x=value, fill=label)) + " +
      "geom_histogram(alpha=.5, position=\"identity\")")

    R.set("plotFilename", f.getAbsolutePath())
    R.eval("ggsave(plot, file = plotFilename)")
  }

}
