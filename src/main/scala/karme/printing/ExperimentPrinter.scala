package karme.printing

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import karme.Experiment
import karme.Measurement
import karme.parsing.ExperimentParser

object ExperimentPrinter {
  def print[MT <: Measurement[_]](e: Experiment[_,MT], f: File): Unit = {
    val writer = CSVWriter.open(f)

    val headerRow = ExperimentParser.ID_LABEL +: e.names
    val cellRows = e.measurements.map(m => m.id +: m.values)

    writer.writeAll(headerRow +: cellRows)
  }
}
