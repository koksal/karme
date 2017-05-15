package karme.printing

import java.io.File

import com.github.tototoshi.csv.CSVWriter

object IOPairLogger {

  def apply(pairs: Set[(String, String)], f: File): Unit = {
    val writer = CSVWriter.open(f)

    val headerRow = List("source", "target")

    val tuples = pairs.toSeq map {
      case (s, t) => List(s, t)
    }

    writer.writeAll(headerRow +: tuples)
  }

}
