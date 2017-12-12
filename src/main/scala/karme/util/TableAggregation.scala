package karme.util

import java.io.File

object TableAggregation {

  def main(args: Array[String]): Unit = {
    val (outFileName, inFileNames) = (args.head, args.tail)
    val data = inFileNames map { n =>
      TSVUtil.readHeadersAndData(new File(n))
    }

    val headers = data.map(_._1)
    assert(headers.toSet.size == 1)

    val tables = inFileNames.zip(data.map(_._2)) map {
      case (name, table) => {
        table.zipWithIndex map {
          case (row, i) => row + ("Run" -> s"$name ($i)")
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

}
