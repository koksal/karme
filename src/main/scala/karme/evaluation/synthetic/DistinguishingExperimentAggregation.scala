package karme.evaluation.synthetic

import java.io.File

import karme.util.DataAggregation
import karme.util.TSVUtil
import karme.visualization.BoxPlot

object DistinguishingExperimentAggregation {

  def main(args: Array[String]): Unit = {
    val files = args.map(a => new File(a))

    var pointToPairs = files map { f =>
      (DataAggregation.grandParentTo2DPoint(f), extractPairs(f))
    }

    pointToPairs = pointToPairs filter {
      case (point, pairs) => {
        pairs.exists(pair => pair._2 > 0)
      }
    }
    val allPairs = pointToPairs flatMap {
      case (point, pairs) => pairs.map(pair => (point, pair))
    }

    val maxPairs = pointToPairs map {
      case (point, pairs) => (point, pairs.maxBy(_._2))
    }

    processPairs(allPairs, new File("all-distinguishing-numbers.pdf"))
    processPairs(maxPairs, new File("max-distinguishing-numbers.pdf"))
  }

  private def extractPairs(f: File): Iterable[(String, Double)] = {
    val (headers, tuples) = TSVUtil.readHeadersAndData(f)
    tuples.map(
      t => (t("Variable"), t("Maximum pairwise distance").toDouble))
  }

  def processPairs(
    pointLabelValues: Iterable[(Map[String, Double], (String, Double))],
    f: File
  ) = {
    val (zeroData, nonZeroData) = pointLabelValues.partition(_._2._2 == 0)

    println(s"Nb. all pairs: ${pointLabelValues.size}")
    println(s"Nb. zero pairs: ${zeroData.size}")
    println(s"Nb. nonzero pairs: ${nonZeroData.size}")

    val labelToValues = pointLabelValues.groupBy(_._2._1).toList.map {
      case (label, labelData) => {
        val labelPairs = labelData.map {
          d => {
            val color = DataAggregation.twoDimPointToColor(d._1)
            val value = d._2._2
            (value, color)
          }
        }
        (label, labelPairs.toList)
      }
    }

    new BoxPlot().plotWithColors(
      labelToValues,
      "Knockout variable",
      "Distinguishing factor",
      f
    )
  }
}
