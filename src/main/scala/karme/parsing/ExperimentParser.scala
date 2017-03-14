package karme.parsing

import java.io.File

import com.github.tototoshi.csv.CSVReader
import karme.Experiments.Experiment
import karme.Experiments.Measurement
import karme.synthesis.Transitions.GenericState
import karme.util.NamingUtil

object ExperimentParser {
  val ID_LABEL = "id"
}

abstract class ExperimentParser[T] {

  def makeValue(s: String): T

  def parseAndFilter(
    f: File, namesToFilterOpt: Option[Set[String]]
  ): Experiment[T] = {
    val reader = CSVReader.open(f)
    val allRows = reader.all()
    val headers = allRows.head
    val cellRows = allRows.tail

    assert(headers.size > 1)
    assert(headers.head == ExperimentParser.ID_LABEL)

    val experimentNames = headers.tail
    val projectionNames = namesToFilterOpt match {
      case None => experimentNames.toSet
      case Some(fns) => NamingUtil.selectNames(experimentNames.toSet, fns)
    }
    println(s"Total number of names in experiment: ${experimentNames.size}")
    println(s"Reading ${projectionNames.size} names from experiment.")

    val measurements = cellRows map { row =>
      val id = row.head
      val values = row.tail.map(makeValue)
      val mapping = experimentNames.zip(values).filter {
        case (n, _) => projectionNames.contains(n)
      }
      val state = GenericState[T](mapping.toMap)
      Measurement(id, state)
    }

    Experiment(measurements)
  }

}

object ContinuousExperimentParser extends ExperimentParser[Double] {
  override def makeValue(s: String): Double = s.toDouble
}

object BooleanExperimentParser extends ExperimentParser[Boolean] {
  override def makeValue(s: String): Boolean = s.toBoolean
}

