package karme.visualization

import java.io.File

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

object BoxPlot extends
  AbstractRInterface[(Map[String, Iterable[Double]], String, File), Unit] {

  val libraries = Seq("ggplot2")

  def run(
    labelToValues: Map[String, Iterable[Double]],
    outPrefix: String,
    outFolder: File
  ): Unit = {
    run((labelToValues, outPrefix, outFolder))
  }

  def process(R: RClient)(
    arg: (Map[String, Iterable[Double]], String, File)
  ): Unit = {
    val (labelToValues, outPrefix, outFolder) = arg

    var dataList = List[Double]()
    var labelArray = Array[String]()

    for ((label, vs) <- labelToValues) {
      dataList = dataList ::: vs.toList
      labelArray = labelArray ++ (1 to vs.size).map(x => label)
    }

    R.set("values", dataList.toArray)
    R.set("labels", labelArray)
    R.eval("data <- data.frame(value = values, label = labels)")

    R.eval("plot = ggplot(data, aes(label, value)) + geom_boxplot()")

    val folder = new File(outFolder, "box-plots")
    folder.mkdirs()
    val f = new File(folder, s"$outPrefix.pdf")

    R.set("fname", f.getAbsolutePath())
    R.eval("ggsave(plot, file = fname)")
  }

}
