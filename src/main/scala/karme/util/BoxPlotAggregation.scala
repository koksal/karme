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
    assert(headers.contains("TPR"))
    assert(headers.contains("FPR"))

    val valueDataPairs = labels.zip(data.map(_._2))

    val tprData = valueDataPairs map {
      case (key, value) => key -> value.map(v => v("TPR").toDouble)
    }
    val fprData = valueDataPairs map {
      case (key, value) => key -> value.map(v => v("FPR").toDouble)
    }

    new BoxPlot().plot(tprData, new File(s"$outFilePrefix-tpr.pdf"))
    new BoxPlot().plot(fprData, new File(s"$outFilePrefix-fpr.pdf"))
  }



}
