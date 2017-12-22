package karme.util

import java.io.File

object TableAggregation {

  def main(args: Array[String]): Unit = {
    val (outFilenamePrefix, inFileNames) = (args.head, args.tail)
    val data = inFileNames map { n =>
      TSVUtil.readHeadersAndData(new File(n))
    }

    val headerRows = data.map(_._1)
    assert(headerRows.toSet.size == 1)
    val headers = headerRows.head

    val runIDDataPairs = inFileNames.zip(data.map(_._2))

    val medianData = runIDDataPairs map {
      case (runID, data) => medianRowForRun(runID, headers, data)
    }

    val nonAggregateData = runIDDataPairs flatMap {
      case (runID, data) => nonAggregateRowsForRun(runID, headers, data)
    }

    val outHeader = "Run" :: headers

    TSVUtil.saveTupleMapsWithOrderedHeaders(
      outHeader,
      medianData,
      new File(s"$outFilenamePrefix-median.tsv")
    )

    TSVUtil.saveTupleMapsWithOrderedHeaders(
      outHeader,
      nonAggregateData,
      new File(s"$outFilenamePrefix-all.tsv")
    )
  }

  def medianRowForRun(
    runID: String,
    headers: Seq[String],
    data: Seq[Map[String, String]]
  ): Map[String, Any] = {
    val medianRow = takeMedian(headers, data)
    medianRow.updated("Run", runID)
  }

  def nonAggregateRowsForRun(
    runID: String,
    headers: Seq[String],
    data: Seq[Map[String, String]]
  ): Seq[Map[String, Any]] = {
    data map { row => row.updated("Run", runID)}
  }

  def takeMedian(
    header: Seq[String],
    rows: Seq[Map[String, String]]
  ): Map[String, Any] = {
    val headerToValues = header map { h =>
      h -> (rows map (r => r(h).toDouble))
    }
    headerToValues.map{
      case (h, vs) => {
        val median = MathUtil.median(vs)
        if (median.toInt == median) {
          h -> median.toInt
        } else {
          h -> MathUtil.roundTo(4)(median)
        }
      }
    }.toMap
  }

}
