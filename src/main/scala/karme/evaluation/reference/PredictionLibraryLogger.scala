package karme.evaluation.reference

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import karme.ReferencePrediction

object PredictionLibraryLogger {

  val SOURCE_FIELD = "source"
  val TARGET_FIELD = "target"
  val WEIGHT_FIELD = "weight"

  def saveToFile(predictions: Seq[ReferencePrediction], f: File): Unit = {
    val writer = CSVWriter.open(f)

    val headerRow = List(SOURCE_FIELD, TARGET_FIELD, WEIGHT_FIELD)

    val dataRows = predictions map {
      case ReferencePrediction(term, target, combinedScore) => {
        List(term, target, combinedScore)
      }
    }

    writer.writeAll(headerRow +: dataRows)
  }

}
