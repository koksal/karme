package karme.evaluation.enrichr

import java.io.File

import com.github.tototoshi.csv.CSVReader

object EnrichrPredictionLibraryParser {

  def apply(
    f: File, maxNbPredictions: Option[Int]
  ): EnrichrPredictionLibrary = {
    val parsedPredictions = parsePredictions(f)

    val predictionsWithDescendingScore =
      parsedPredictions.sortBy(_.combinedScore).reverse

    val sizeLimitedPredictions = maxNbPredictions match {
      case Some(maxNb) => predictionsWithDescendingScore take maxNb
      case None => predictionsWithDescendingScore
    }

    EnrichrPredictionLibrary(f.getPath, sizeLimitedPredictions)
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
