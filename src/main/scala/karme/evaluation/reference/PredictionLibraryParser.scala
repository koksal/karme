package karme.evaluation.reference

import java.io.File

import com.github.tototoshi.csv.CSVReader
import karme.util.{FileUtil, NamingUtil}
import karme.{PredictionLibrary, ReferencePrediction}

object PredictionLibraryParser {

  def apply(f: File): PredictionLibrary = {
    val parsedPredictions = parsePredictions(f)

    val predsByDescWeight = parsedPredictions.sortBy(- _.weight)

    PredictionLibrary(FileUtil.getFileName(f.getPath), predsByDescWeight)
  }

  private def parsePredictions(f: File): Seq[ReferencePrediction] = {
    val reader = CSVReader.open(f)
    val tuples = reader.allWithHeaders()

    tuples map { tuple =>
      val source = tuple(PredictionLibraryLogger.SOURCE_FIELD)
      val target = tuple(PredictionLibraryLogger.TARGET_FIELD)
      val weight = tuple(PredictionLibraryLogger.WEIGHT_FIELD)
      ReferencePrediction(
        NamingUtil.canonicalize(source),
        NamingUtil.canonicalize(target),
        weight.toDouble
      )
    }
  }

}
