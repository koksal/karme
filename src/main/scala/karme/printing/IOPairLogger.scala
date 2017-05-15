package karme.printing

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import karme.util.CollectionUtil

object IOPairLogger {

  def logPairs(pairs: Seq[(String, String)], f: File): Unit = {
    val pairsWithCounts = CollectionUtil.orderByCount(pairs)

    logPairsWithCounts(pairsWithCounts, f)
  }

  def logPairsWithCounts(pairs: Seq[((String, String), Int)], f: File): Unit = {
    val writer = CSVWriter.open(f)

    val headerRow = List("source", "target", "count")

    val tuples = pairs.map {
      case ((s, t), count) => List(s, t, count.toString)
    }

    writer.writeAll(headerRow +: tuples)
    writer.close()
  }
}
