package karme.evaluation.enrichr

import java.io.File

import com.github.tototoshi.csv.CSVWriter

object EnrichrPredictionLogger {

  val SOURCE_FIELD = "source"
  val TARGET_FIELD = "target"
  val COMBINED_SCORE_FIELD = "Combined Score"

  def saveToFile(predictions: Seq[EnrichrPrediction], f: File): Unit = {
    val writer = CSVWriter.open(f)

    val headerRow = List(SOURCE_FIELD, TARGET_FIELD, COMBINED_SCORE_FIELD)

    val dataRows = predictions map {
      case EnrichrPrediction(term, target, combinedScore) => {
        List(term, target, combinedScore)
      }
    }

    writer.writeAll(headerRow +: dataRows)
  }

}
