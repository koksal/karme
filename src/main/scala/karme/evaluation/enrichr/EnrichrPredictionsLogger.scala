package karme.evaluation.enrichr

import java.io.File

import com.github.tototoshi.csv.CSVWriter

object EnrichrPredictionsLogger {

  def saveToFile(predictions: Seq[EnrichrPrediction], f: File): Unit = {
    val writer = CSVWriter.open(f)

    val headerRow = List("source", "target", "Combined Score")

    val dataRows = predictions map {
      case EnrichrPrediction(term, target, combinedScore) => {
        List(term, target, combinedScore)
      }
    }

    writer.writeAll(headerRow +: dataRows)
  }

}
