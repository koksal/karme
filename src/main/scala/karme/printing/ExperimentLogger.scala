package karme.printing

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import karme.Experiments.Experiment
import karme.parsing.ExperimentParser

object ExperimentLogger {

  def saveToFile[T](
    e: Experiment[T], f: File, cellToNodeIDOpt: Option[Map[String, String]]
  ): Unit = {
    val writer = CSVWriter.open(f)

    val headerRow = cellToNodeIDOpt match {
      case Some(_) => List(ExperimentParser.ID_LABEL, "NODE_ID") ++ e.names
      case None => ExperimentParser.ID_LABEL +: e.names
    }

    val cellRows = e.measurements map { m =>
      cellToNodeIDOpt match {
        case Some(cellToNodeID) => {
          List(m.id, cellToNodeID(m.id)) ++ m.state.orderedValues
        }
        case None => m.id +: m.state.orderedValues
      }
    }

    writer.writeAll(headerRow +: cellRows)
  }

}
