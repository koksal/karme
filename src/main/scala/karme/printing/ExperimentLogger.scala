package karme.printing

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import karme.Experiments.Experiment
import karme.parsing.ExperimentParser

object ExperimentLogger {

  def saveToFile[T](e: Experiment[T], f: File): Unit = {
    val writer = CSVWriter.open(f)

    val headerRow = ExperimentParser.ID_LABEL +: e.names

    val cellRows = e.measurements map { m =>
      m.id +: m.state.orderedValues
    }

    writer.writeAll(headerRow +: cellRows)
  }

}
