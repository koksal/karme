package karme.parsing

import java.io.File

import com.github.tototoshi.csv.CSVReader
import karme.ContinuousCellMeasurement
import karme.ContinuousExperiment

object ContinuousExperimentParser {

  private val ID_LABEL = "id"

  def parse(f: File): ContinuousExperiment = {
    val reader = CSVReader.open(f)
    val (headers, rows) = reader.allWithOrderedHeaders()

    assert(headers.size > 1)
    assert(headers.head == ID_LABEL)

    val names = headers.tail

    val measurements = rows map { row =>
      val id = row(ID_LABEL)
      val values = names map { n => row(n).toDouble }
      ContinuousCellMeasurement(id, values)
    }

    ContinuousExperiment(names, measurements)
  }
}
