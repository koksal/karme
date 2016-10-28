package karme.parsing

import java.io.File

import com.github.tototoshi.csv.CSVReader
import karme.CellTrajectories.CellTrajectory

object CellTrajectoryParser {

  def parse(f: File): CellTrajectory = {
    val reader = CSVReader.open(f)
    val allRows = reader.all()
    val headers = allRows.head
    val data = allRows.tail

    assert(headers == List("id", "pseudotime"))

    var mapping = Map[String, Double]()

    for (List(id, pt) <- data) {
      try {
        mapping += id -> (pt.toDouble)
      } catch {
        case e: NumberFormatException => // skip this cell
      }
    }

    mapping
  }

}
