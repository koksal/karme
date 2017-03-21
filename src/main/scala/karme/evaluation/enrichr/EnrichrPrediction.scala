package karme.evaluation.enrichr

case class EnrichrPredictionLibrary(
  id: String,
  predictions: Seq[EnrichrPrediction]
)

case class EnrichrPrediction(
  term: String, target: String, combinedScore: Double
)
