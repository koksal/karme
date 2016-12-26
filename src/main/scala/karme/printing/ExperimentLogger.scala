package karme.printing

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import karme.Experiments.Experiment
import karme.parsing.ExperimentParser

object ExperimentLogger {

  def saveToFile[T](
    e: Experiment[T], cellToNodeID: Map[String, String], f: File
  ): Unit = {
    val writer = CSVWriter.open(f)

    val headerRow = List(ExperimentParser.ID_LABEL, "NODE_ID") ++ e.names
    val cellRows = e.measurements map { m =>
      List(m.id, cellToNodeID(m.id)) ++ m.state.orderedValues
    }

    writer.writeAll(headerRow +: cellRows)
  }

}
