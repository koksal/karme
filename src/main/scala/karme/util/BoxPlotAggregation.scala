package karme.util

import java.io.File

import karme.visualization.BoxPlot

object BoxPlotAggregation {

  def main(args: Array[String]): Unit = {
    val (outFilePrefix, restArgs) = (args.head, args.tail)

    val fnames = restArgs.sorted
    val files = fnames.map(a => new File(a))
    val keyValuePairs = files map DataAggregation.grandParentToKeyValue

    val labels = keyValuePairs.map(_._2)
    val sortedUniqueLabels = labels.distinct.sorted

    val data = files map TSVUtil.readHeadersAndData

    val headerRows = data.map(_._1)
    assert(headerRows.toSet.size == 1,
      s"Different headers: ${headerRows.toSet}")
    val headers = headerRows.head
    val headersToAggregate = headers.toSet.intersect(
      DataAggregation.columnsToAggregate)

    val labelDataPairs = labels.zip(data.map(_._2))

    for (header <- headersToAggregate) {
      val labelToAllValues = for (label <- sortedUniqueLabels) yield {
        val allValues = labelDataPairs.filter(_._1 == label).flatMap {
          case (_, rows) =>
            rows.map(row => row(header).toDouble)
        }

        label -> allValues.toList
      }
      new BoxPlot().plot(
        labelToAllValues,
        "Hidden variable",
        header,
        new File(s"$outFilePrefix-$header-boxplot.pdf"))
    }
  }

}
