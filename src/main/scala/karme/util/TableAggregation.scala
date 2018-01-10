package karme.util

import java.io.File

object TableAggregation {

  def main(args: Array[String]): Unit = {
    val (outFilename, inFileNames) = (args.head, args.tail)
    val data = inFileNames map { n =>
      TSVUtil.readHeadersAndData(new File(n))
    }

    val headerRows = data.map(_._1)
    assert(headerRows.toSet.size == 1)
    val headers = headerRows.head

    val runIDs = inFileNames map stripRunID

    val runIDDataPairs = runIDs.zip(data.map(_._2))

    val nonAggregateData = runIDDataPairs flatMap {
      case (runID, data) => nonAggregateRowsForRun(runID, headers, data)
    }

    TSVUtil.saveTupleMapsWithOrderedHeaders(
      "Model" :: headers,
      nonAggregateData,
      new File(outFilename)
    )
  }

  def nonAggregateRowsForRun(
    runID: String,
    headers: Seq[String],
    data: Seq[Map[String, String]]
  ): Seq[Map[String, Any]] = {
    data.zipWithIndex map {
      case (row, i) => {
        row.updated("Model", s"$runID (${i + 1})")
      }
    }
  }

  def stripRunID(rawID: String): String = {
    val splitted = rawID.split("/")
    splitted(splitted.size - 2)
      .replaceAll("[-_]", " ")
      .capitalize
  }

}
