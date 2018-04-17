package karme.evaluation.synthetic

import java.io.File

import karme.util.TSVUtil
import karme.visualization.BoxPlot

object DistinguishingExperimentAggregation {

  def main(args: Array[String]): Unit = {
    val files = args.map(a => new File(a))

    val pairsByFile = files map { f =>
      val (headers, tuples) = TSVUtil.readHeadersAndData(f)
      tuples.map(
        t => (t("Variable"), t("Maximum pairwise distance").toDouble))
    }

    val allPairs = pairsByFile.flatten
    val maxPairs = pairsByFile map {
      pairs => pairs.maxBy(_._2)
    }

    processPairs(allPairs, new File("all-distinguishing-numbers.pdf"))
    processPairs(maxPairs, new File("max-distinguishing-numbers.pdf"))
  }

  def processPairs(
    pairs: Iterable[(String, Double)],
    f: File
  ) = {
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
      f
    )
  }
}
