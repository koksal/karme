package karme.visualization

import java.io.File

import org.ddahl.rscala.RClient

import scala.reflect.ClassTag

object ScatterPlot {

  def plot[T: ClassTag, U: ClassTag](
    labeledPoints: Iterable[(T, U, String)],
    file: File
  ): Unit = {
    val R = RClient()
    R eval "library(ggplot2)"

    val (xs, ys, ls) = labeledPoints.unzip3
    R.set("xs", xs.toArray)
    R.set("ys", ys.toArray)

    R.set("labels", ls.toArray)
    R.eval("data <- data.frame(x = xs, y = ys, label = labels)")

    val aes = "aes(x = x, y = y, color = label)"
    R.eval(s"plot = ggplot(data, $aes) + geom_line() + " +
      "  theme(" +
      "     axis.text.y=element_blank(), " +
      "     axis.ticks.y=element_blank())")

    R.set("fname", file.getAbsolutePath())
    R.eval("ggsave(plot, file = fname)")

    R.exit()
  }

}
