package karme.visualization

import java.io.File

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

import scala.reflect.ClassTag

trait PlotStyle
case object GeomPoint extends PlotStyle
case object GeomLine extends PlotStyle

class ScatterPlot extends AbstractRInterface {

  override def LIBRARIES: Seq[String] = List("ggplot2", "scales")

  def plot[T: ClassTag, U: ClassTag](
    labeledPoints: Iterable[(T, U, String)],
    file: File,
    logYScale: Boolean = false,
    plotStyle: PlotStyle = GeomLine
  ): Unit = {
    val (xs, ys, ls) = labeledPoints.unzip3
    R.set("xs", xs.toArray)
    R.set("ys", ys.toArray)

    R.set("labels", ls.toArray)
    R.eval("data <- data.frame(x = xs, y = ys, label = labels)")

    val yScaleStr = if (logYScale) {
       " + scale_y_continuous(trans=log10_trans())"
    } else {
      ""
    }

    val styleStr = plotStyle match {
      case GeomPoint => "geom_point()"
      case GeomLine => "geom_line()"
    }

    val aes = "aes(x = x, y = y, color = label)"
    R.eval(s"plot = ggplot(data, $aes) + $styleStr + " +
      s"expand_limits(x = 0, y = 0)$yScaleStr")

    R.set("fname", file.getAbsolutePath())
    R.eval("ggsave(plot, file = fname)")
  }

}
