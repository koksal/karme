package karme.visualization

import java.io.File

import karme.external.AbstractRInterface
import karme.util.CollectionUtil

import scala.util.Random

class BoxPlot extends AbstractRInterface {

  override def LIBRARIES = Seq("ggplot2")

  def plotWithColors(
    labelToValues: Iterable[(String, Iterable[(Double, String)])],
    xName: String,
    yName: String,
    f: File
  ): Unit = {
    var labels = List[String]()
    var values = List[Double]()
    var colors = List[String]()

    for ((label, valueColorPairs) <- labelToValues) {
      val (vs, cs) = valueColorPairs.unzip
      values = values ::: vs.toList
      colors = colors ::: cs.toList
      labels = labels ::: (1 to vs.size).map(x => label).toList
    }

    randomizeOrderAndPlot(labels, values, Some(colors), xName, yName, f)
  }

  def plot(
    labelToValues: Iterable[(String, Iterable[Double])],
    xName: String,
    yName: String,
    f: File
  ): Unit = {
    var labels = List[String]()
    var values = List[Double]()

    for ((label, vs) <- labelToValues) {
      values = values ::: vs.toList
      labels = labels ::: (1 to vs.size).map(x => label).toList
    }

    randomizeOrderAndPlot(labels, values, None, xName, yName, f)
  }

  private def randomizeOrderAndPlot(
    labels: List[String],
    values: List[Double],
    colorsOpt: Option[List[String]],
    xName: String,
    yName: String,
    f: File
  ): Unit = {
    val permutation =
      CollectionUtil.randomPermutation(new Random())(labels.size)

    plot(
      CollectionUtil.permuteElements(labels, permutation),
      CollectionUtil.permuteElements(values, permutation),
      colorsOpt.map(
        colors => CollectionUtil.permuteElements(colors, permutation)),
      xName,
      yName,
      f
    )
  }

  private def plot(
    labels: Seq[String],
    values: Seq[Double],
    colorsOpt: Option[Seq[String]],
    xName: String,
    yName: String,
    f: File
  ): Unit = {
    assert(labels.size == values.size)
    colorsOpt match {
      case Some(colors) => assert(labels.size == colors.size)
      case None =>
    }

    R.set("values", values.toArray)
    R.set("labels", labels.toArray)

    colorsOpt match {
      case Some(colors) => {
        R.set("colors", colors.toArray)
      }
      case None =>
    }

    val colStr = if (colorsOpt.isDefined) {
      ", color = colors"
    } else {
      ""
    }

    R.eval(s"data <- data.frame(value = values, label = labels $colStr)")

    R.eval(s"""plot =
              | ggplot(
              |   data,
              |   aes(label, value $colStr)
              |   ) +
              |   xlab("$xName") +
              |   ylab("$yName") +
              |   labs(color = "Data quality") +
              |   geom_jitter(size = 0.8, height = 0.1) +
              |   geom_boxplot(alpha = 0, color = \"black\",
              |     outlier.alpha = 0) +
              |   scale_color_brewer(palette="RdBu")"""
      .stripMargin)

    R.set("fname", f.getAbsolutePath())
    R.eval("ggsave(plot, file = fname)")
  }

}
