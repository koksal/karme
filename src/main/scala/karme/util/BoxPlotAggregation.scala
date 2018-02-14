package karme.util

import java.io.File

import karme.visualization.BoxPlot

object BoxPlotAggregation {

  def main(args: Array[String]): Unit = {
    val (outFilePrefix, restArgs) = (args.head, args.tail)

    val fnames = restArgs.sorted
    val files = fnames.map(a => new File(a))
    val labels = files.map(f => f.getAbsoluteFile.getParentFile.getName)

    val data = files map TSVUtil.readHeadersAndData

    val headerRows = data.map(_._1)
    assert(headerRows.toSet.size == 1,
      s"Different headers: ${headerRows.toSet}")
    val headers = headerRows.head
    val headersToAggregate = headers.toSet.intersect(
      DataAggregation.columnsToAggregate)

    val valueDataPairs = labels.zip(data.map(_._2))

    for (header <- headersToAggregate) {
      val headerData = valueDataPairs map {
        case (key, value) => key -> value.map(v => v(header).toDouble)
      }
      new BoxPlot().plot(headerData,
        new File(s"$outFilePrefix-$header-boxplot.pdf"))
    }
  }

}
