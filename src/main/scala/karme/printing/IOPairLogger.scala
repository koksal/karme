package karme.printing

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import karme.util.CollectionUtil

object IOPairLogger {

  def apply(pairs: Seq[(String, String)], f: File): Unit = {
    val writer = CSVWriter.open(f)

    val headerRow = List("source", "target", "count")

    val pairsWithCounts = CollectionUtil.orderByCount(pairs)

    val tuples = pairsWithCounts.map {
      case ((s, t), count) => List(s, t, count.toString)
    }

    writer.writeAll(headerRow +: tuples)
  }

}
