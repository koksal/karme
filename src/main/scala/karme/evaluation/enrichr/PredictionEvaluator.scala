package karme.evaluation.enrichr

import karme.synthesis.SynthesisResult

class PredictionEvaluator(referencePredictions: Seq[EnrichrPrediction]) {

  def compareToReferences(
    results: Seq[Map[String, SynthesisResult]],
    clustering: Option[Map[String, Set[String]]]
  ): Unit = {
    // For each target, gather possible sources
    // Map cluster-level pairs to gene level
    // Check against randomized data
  }

}
