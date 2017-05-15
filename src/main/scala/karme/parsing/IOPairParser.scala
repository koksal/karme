package karme.parsing

import java.io.File

import com.github.tototoshi.csv.CSVReader

object IOPairParser {

  def apply(f: File): Seq[((String, String), Int)] = {
    val reader = CSVReader.open(f)
    val allRows = reader.all()
    val headers = allRows.head
    val tuples = allRows.tail

    assert(headers == List("source", "target", "count"))

    tuples map {
      case List(source, target, count) => ((source, target), count.toInt)
    }
  }

}
