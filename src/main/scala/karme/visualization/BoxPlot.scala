package karme.visualization

import java.io.File

import karme.external.AbstractRInterface

class BoxPlot extends AbstractRInterface {

  override def LIBRARIES = Seq("ggplot2")

  def plot(
    labelToValues: Iterable[(String, Iterable[Double])],
    f: File
  ): Unit = {
    var dataList = List[Double]()
    var labelArray = Array[String]()

    for ((label, vs) <- labelToValues) {
      dataList = dataList ::: vs.toList
      labelArray = labelArray ++ (1 to vs.size).map(x => label)
    }

    R.set("values", dataList.toArray)
    R.set("labels", labelArray)
    R.eval("data <- data.frame(value = values, label = labels)")

    R.eval("plot = ggplot(data, aes(label, value)) + geom_boxplot() " +
      "+ geom_jitter(height = 0.0)")

    R.set("fname", f.getAbsolutePath())
    R.eval("ggsave(plot, file = fname)")
  }

}
