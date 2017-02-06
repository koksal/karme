package karme.visualization

import java.io.File

import org.ddahl.rscala.RClient

object ScatterPlot {

  def plot[T](
    xs: Iterable[T],
    ys: Iterable[T],
    file: File
  ): Unit = {
    assert(xs.size == ys.size)

    val R = RClient()
    R eval "library(ggplot2)"

    R.set("xs", xs.toArray)
    R.set("ys", ys.toArray)

    R.eval("data <- data.frame(x = xs, y = ys)")

    R.eval("plot = ggplot(data, aes(x = x, y = y)) + geom_point() + " +
      "  theme(" +
      "     axis.text.y=element_blank(), " +
      "     axis.ticks.y=element_blank())")

    R.set("fname", file.getAbsolutePath())
    R.eval("ggsave(plot, file = fname)")

    R.exit()
  }

}
