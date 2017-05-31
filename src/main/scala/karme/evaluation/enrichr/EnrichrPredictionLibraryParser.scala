package karme.evaluation.enrichr

import java.io.File

import com.github.tototoshi.csv.CSVReader
import karme.util.FileUtil

object EnrichrPredictionLibraryParser {

  def apply(f: File): EnrichrPredictionLibrary = {
    val parsedPredictions = parsePredictions(f)

    val predictionsWithDescendingScore =
      parsedPredictions.sortBy(_.combinedScore).reverse

    EnrichrPredictionLibrary(FileUtil.getFileName(f.getPath),
      predictionsWithDescendingScore)
  }

  private def parsePredictions(f: File): Seq[ReferencePrediction] = {
    val reader = CSVReader.open(f)
    val tuples = reader.allWithHeaders()

    tuples map { tuple =>
      val source = tuple(EnrichrPredictionLogger.SOURCE_FIELD)
      val target = tuple(EnrichrPredictionLogger.TARGET_FIELD)
      val score = tuple(EnrichrPredictionLogger.COMBINED_SCORE_FIELD)
      ReferencePrediction(source, target, score.toDouble)
    }
  }

}
