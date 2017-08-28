package karme.evaluation.enrichr

import java.io.File

import com.github.tototoshi.csv.CSVReader
import karme.{PredictionLibrary, ReferencePrediction}
import karme.util.FileUtil

object EnrichrPredictionLibraryParser {

  def apply(f: File): PredictionLibrary = {
    val parsedPredictions = parsePredictions(f)

    val predsByDescWeight = parsedPredictions.sortBy(- _.weight)

    PredictionLibrary(FileUtil.getFileName(f.getPath), predsByDescWeight)
  }

  private def parsePredictions(f: File): Seq[ReferencePrediction] = {
    val reader = CSVReader.open(f)
    val tuples = reader.allWithHeaders()

    tuples map { tuple =>
      val source = tuple(EnrichrPredictionLogger.SOURCE_FIELD)
      val target = tuple(EnrichrPredictionLogger.TARGET_FIELD)
      val weight = tuple(EnrichrPredictionLogger.WEIGHT_FIELD)
      ReferencePrediction(source, target, weight.toDouble)
    }
  }

}
