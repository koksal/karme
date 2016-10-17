package karme.parsing

import java.io.File

import com.github.tototoshi.csv.CSVReader
import karme.ContinuousCellMeasurement
import karme.ContinuousExperiment
import karme.DiscreteCellMeasurement
import karme.DiscreteExperiment

object ExperimentParser {

  val ID_LABEL = "id"

}

abstract class ExperimentParser[ET, MT, VT](
  makeValue: String => VT,
  makeMeasurement: (String, Seq[VT]) => MT,
  makeExperiment: (Seq[String], Seq[MT]) => ET
) {

  def parse(f: File): ET = {
    val reader = CSVReader.open(f)
    val allRows = reader.all()
    val headers = allRows.head
    val cellRows = allRows.tail

    assert(headers.size > 1)
    assert(headers.head == ExperimentParser.ID_LABEL)

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
  (id: String, values: Seq[Double]) => ContinuousCellMeasurement(id, values),
  (names: Seq[String], measurements: Seq[ContinuousCellMeasurement]) =>
    ContinuousExperiment(names, measurements)
)

object DiscreteExperimentParser extends ExperimentParser(
  x => x.toInt,
  (id: String, values: Seq[Int]) => DiscreteCellMeasurement(id, values),
  (names: Seq[String], measurements: Seq[DiscreteCellMeasurement]) =>
    DiscreteExperiment(names, measurements)
)
