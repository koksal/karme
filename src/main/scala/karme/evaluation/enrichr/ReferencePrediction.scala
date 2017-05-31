package karme.evaluation.enrichr

case class EnrichrPredictionLibrary(
  id: String,
  predictions: Seq[ReferencePrediction]
) {

  def names: Set[String] = {
    (predictions flatMap {
      case ReferencePrediction(term, target, _) => Set(term, target)
    }).toSet
  }

  def ioPairs: Set[(String, String)] = {
    predictions.map{
      case ReferencePrediction(term, target, _) => (term, target)
    }.toSet
  }

}

case class ReferencePrediction(
  term: String, target: String, combinedScore: Double
)
