package karme.evaluation.synthetic

import java.io.File

import karme.util.FileUtil
import karme.util.TSVUtil
import karme.visualization.BoxPlot

object DistinguishingExperimentAggregation {

  def main(args: Array[String]): Unit = {
    val files = args.map(a => new File(a))

    val pairs = files flatMap { f =>
      val (headers, tuples) = TSVUtil.readHeadersAndData(f)
      tuples.map(
        t => (t("Variable"), t("Maximum pairwise distance").toDouble))
    }

    val (zeroPairs, nonZeroPairs) = pairs.partition(_._2 == 0)

    println(s"Nb. all pairs: ${pairs.size}")
    println(s"Nb. zero pairs: ${zeroPairs.size}")
    println(s"Nb. nonzero pairs: ${nonZeroPairs.size}")

    val labelToValues = pairs.groupBy(_._1).toList.map {
      case (name, pairs) => (name, pairs.map(_._2).toList)
    }

    new BoxPlot().plot(
      labelToValues,
      "Knockout variable",
      "Distinguishing factor",
      new File("test.pdf")
    )
  }
}
