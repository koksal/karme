package karme.parsing

import java.io.File

import com.github.tototoshi.csv.CSVReader
import karme.ContinuousCellMeasurement
import karme.ContinuousExperiment
import karme.DiscreteCellMeasurement
import karme.DiscreteExperiment

abstract class ExperimentParser[ET, MT, VT](
  makeValue: String => VT,
  makeMeasurement: (String, Seq[VT]) => MT,
  makeExperiment: (Seq[String], Seq[MT]) => ET
) {

  private val ID_LABEL = "id"

  def parse(f: File): ET = {
    val reader = CSVReader.open(f)
    val allRows = reader.all()
    val headers = allRows.head
    val cellRows = allRows.tail

    assert(headers.size > 1)
    assert(headers.head == ID_LABEL)

    val names = headers.tail

    val measurements = cellRows map { row =>
      val id = row.head
      val values = row.tail.map(makeValue)
      makeMeasurement(id, values)
    }

    makeExperiment(names, measurements)
  }
}

object ContinuousExperimentParser extends ExperimentParser(
  x => x.toDouble,
  (id, values) => ContinuousCellMeasurement(id, values),
  (names, measurements) => ContinuousExperiment(names, measurements)
)

object DiscreteExperimentParser extends ExperimentParser(
  x => x.toInt,
  (id, values) => DiscreteCellMeasurement(id, values),
  (names, measurements) => DiscreteExperiment(names, measurements)
)
