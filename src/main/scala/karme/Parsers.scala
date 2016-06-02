package karme

import java.io.File
import com.github.tototoshi.csv._

object Parsers {
  def readProteins(f: File): Seq[String] = {
    lines(f)
  }

  def readGNWTimeSeries(f: File): List[Map[String, String]] = {
    val format = new TSVFormat {}
    val reader = CSVReader.open(f)(format)
    reader.allWithHeaders()
  }

  def readExperiment(prots: Seq[String], expFile: File): Experiment = {
    val reader = CSVReader.open(expFile)
    val tuples = reader.allWithHeaders()
    val cellMeasurements = tuples map { tuple =>
      val time = tuple("Minute").toDouble
      val actualTime = tuple.get("ActualTime") match {
        case Some(v) => v.toDouble
        case None => -1.0
      }
      val protValues = prots.map{ prot => tuple(prot).toDouble }.toIndexedSeq
      val pseudotime = -1.0
      CellMeasurement(time, actualTime, pseudotime, protValues)
    }
    Experiment(prots, cellMeasurements.toIndexedSeq)
  }

  def readDouble(f: File): Double = {
    val v = lines(f).head
    try {
      v.toDouble
    } catch {
      case e: java.lang.NumberFormatException => v match {
        case "-Inf" => Double.NegativeInfinity
        case "Inf" => Double.PositiveInfinity
        case _ => Double.NaN
      }
    }
  }

  def lines(f: File): Seq[String] = {
    scala.io.Source.fromFile(f).getLines.toSeq
  }

  def readEMD(imfF: File, residueF: File): (Seq[Seq[Double]], Seq[Double]) = {
    val imfLines = lines(imfF)
    val imfTuples = imfLines.map{ l => l.split(",").toSeq.map(_.toDouble) }
    val imfs = imfTuples.transpose
    val residueLines = lines(residueF)
    val residue = residueLines.map(_.toDouble)

    (imfs, residue)
  }

  def readIntVector(f: File): Seq[Int] = {
    lines(f).map(_.toInt)
  }
}
