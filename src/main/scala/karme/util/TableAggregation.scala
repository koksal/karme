package karme.util

import java.io.File

object TableAggregation {

  val takeMedians = true

  def main(args: Array[String]): Unit = {
    val (outFileName, inFileNames) = (args.head, args.tail)
    val data = inFileNames map { n =>
      TSVUtil.readHeadersAndData(new File(n))
    }

    val headers = data.map(_._1)
    assert(headers.toSet.size == 1)

    val tables = inFileNames.zip(data.map(_._2)) map {
      case (name, table) => {
        if (takeMedians) {
          val medianRow = takeMedian(headers.head, table)
          Seq(medianRow.updated("Run", name))
        } else {
          table map { row => row.updated("Run", name)}
        }
      }
    }

    val outHeader = "Run" :: headers.head

    TSVUtil.saveTupleMapsWithOrderedHeaders(
      outHeader,
      tables.toList.flatten,
      new File(outFileName)
    )
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
