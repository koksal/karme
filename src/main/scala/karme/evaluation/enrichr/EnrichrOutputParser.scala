package karme.evaluation.enrichr

import java.io.File

import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.TSVFormat
import karme.util.NamingUtil

class EnrichrOutputParser(canonicalNames: Set[String]) {

  def parse(f: File): Seq[ReferencePrediction] = {
    val reader = CSVReader.open(f)(new TSVFormat {})
    val tuples = reader.allWithHeaders()
    val predictionOptions = tuples map extractEnrichrPrediction
    predictionOptions collect {
      case Some(prediction) => prediction
    }
  }

  private def extractEnrichrPrediction(
    tuple: Map[String, String]
  ): Option[ReferencePrediction] = {
    val canonicalTerm = NamingUtil.canonicalize(tuple("Term"))
    nameFromCanonicalTerm(canonicalTerm) map { matchedCanonicalTermName =>
      val canonicalTarget = NamingUtil.canonicalize(tuple("Genes"))
      val score = tuple("Combined Score").toDouble
      ReferencePrediction(matchedCanonicalTermName, canonicalTarget, score)
    }
  }

  private def nameFromCanonicalTerm(canonicalTerm: String): Option[String] = {
    // split term at _ or whitespace, take first element.
    val canonicalTermPrefix = canonicalTerm.split("[\\s_]").head

    val matchingNames = canonicalNames filter { cn =>
      cn.equals(canonicalTermPrefix)
    }
    matchingNames.headOption
  }
}
