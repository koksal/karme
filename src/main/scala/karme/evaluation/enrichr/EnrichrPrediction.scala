package karme.evaluation.enrichr

case class EnrichrPredictionLibrary(
  id: String,
  predictions: Seq[EnrichrPrediction]
) {

  def names: Set[String] = {
    (predictions flatMap {
      case EnrichrPrediction(term, target, _) => Set(term, target)
    }).toSet
  }

}

case class EnrichrPrediction(
  term: String, target: String, combinedScore: Double
)
