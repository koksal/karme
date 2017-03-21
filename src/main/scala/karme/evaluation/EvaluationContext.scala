package karme.evaluation

import karme.EvalOpts
import karme.evaluation.enrichr.EnrichrPredictionLibrary
import karme.evaluation.enrichr.EnrichrPredictionParser

case class EvaluationContext(
  predictionLibraries: Seq[EnrichrPredictionLibrary]
)

object EvaluationContext {

  def fromOptions(opts: EvalOpts): EvaluationContext = {
    val parsedLibraries = opts.predictionFiles.map(
      EnrichrPredictionParser.apply)
    EvaluationContext(predictionLibraries = parsedLibraries)
  }

}
