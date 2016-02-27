package karme

import java.io.File
import com.github.tototoshi.csv._

object Parsers {
  def readProteins(f: File): Seq[String] = {
    scala.io.Source.fromFile(f).getLines.toSeq
  }

  def readExperiment(prots: Seq[String], expFile: File): Experiment = {
    val reader = CSVReader.open(expFile)
    val tuples = reader.allWithHeaders()
    val cellMeasurements = tuples map { tuple =>
      val time = tuple("Minute").toDouble
      val protValues = prots.map{ prot => tuple(prot).toDouble }.toIndexedSeq
      CellMeasurement(time, protValues)
    }
    Experiment(prots, cellMeasurements.toIndexedSeq)
  }
}
