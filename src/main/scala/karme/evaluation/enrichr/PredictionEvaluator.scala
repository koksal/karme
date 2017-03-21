package karme.evaluation.enrichr

import karme.synthesis.SynthesisResult

class PredictionEvaluator(referencePredictions: Seq[EnrichrPrediction]) {

  def compareToReferences(results: Seq[Map[String, SynthesisResult]]): Unit = {
    // For each target, gather possible sources
    // Map cluster-level pairs to gene level
    // TODO carry over clustering in context.
    // Check against randomized data
  }

}
