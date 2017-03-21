package karme.evaluation.enrichr

import java.io.File

import com.github.tototoshi.csv.CSVReader

object EnrichrPredictionParser {

  def apply(f: File): EnrichrPredictionLibrary = {
    EnrichrPredictionLibrary(f.getPath, parsePredictions(f))
  }

  private def parsePredictions(f: File): Seq[EnrichrPrediction] = {
    val reader = CSVReader.open(f)
    val tuples = reader.allWithHeaders()

    tuples map { tuple =>
      val source = tuple(EnrichrPredictionLogger.SOURCE_FIELD)
      val target = tuple(EnrichrPredictionLogger.TARGET_FIELD)
      val score = tuple(EnrichrPredictionLogger.COMBINED_SCORE_FIELD)
      EnrichrPrediction(source, target, score.toDouble)
    }
  }

}
