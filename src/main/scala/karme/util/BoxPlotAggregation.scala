package karme.util

import java.io.File

import karme.evaluation.synthetic.ClassificationEval
import karme.visualization.BoxPlot

object BoxPlotAggregation {

  def main(args: Array[String]): Unit = {
    val (outFilePrefix, restArgs) = (args.head, args.tail)

    val fnames = restArgs.sorted
    val files = fnames.map(a => new File(a))

    val keyValuePairs = files map DataAggregation.grandParentToKeyValue
    val twoDimPoints = files map DataAggregation.parentTo2DPoint

    val labels = keyValuePairs.map(_._2)
    val colors = twoDimPoints map DataAggregation.twoDimPointToColor
    val labelColorPairs = labels.zip(colors)

    val sortedUniqueLabels = labels.distinct.sorted

    val data = files map TSVUtil.readHeadersAndData

    val headerRows = data.map(_._1)
    assert(headerRows.toSet.size == 1,
      s"Different headers: ${headerRows.toSet}")
    val headers = headerRows.head
    val headersToAggregate = headers.toSet.intersect(
      DataAggregation.columnsToAggregate)

    val labelColorData = labelColorPairs.zip(data.map(_._2))

    for (header <- headersToAggregate) {
      val labelToAllValues = for (label <- sortedUniqueLabels) yield {
        val allValues = labelColorData.filter(_._1._1 == label).flatMap {
          case ((label, color), rows) =>
            rows.map(row => (row(header).toDouble, color))
        }

        label -> allValues.toList
      }
      new BoxPlot().plotWithColors(
        labelToAllValues,
        "Hidden variable",
        header,
        new File(s"$outFilePrefix-$header-boxplot.pdf"))
    }
  }
}
