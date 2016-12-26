package karme.parsing

import java.io.File

import com.github.tototoshi.csv.CSVReader
import karme.Experiments.Experiment
import karme.Experiments.Measurement
import karme.synthesis.Transitions.GenericState

object ExperimentParser {

  val ID_LABEL = "id"

}

abstract class ExperimentParser[T] {

  def makeValue(s: String): T

  def parse(f: File): Experiment[T] = {
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
      val state = GenericState[T](headers.zip(values).toMap)
      Measurement(id, state)
    }

    Experiment(measurements)
  }
}

object ContinuousExperimentParser extends ExperimentParser[Double] {
  override def makeValue(s: String): Double = s.toDouble
}

object DiscreteExperimentParser extends ExperimentParser[Int] {
  override def makeValue(s: String): Int = s.toInt
}

