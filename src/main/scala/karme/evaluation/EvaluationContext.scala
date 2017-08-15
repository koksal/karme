package karme.evaluation

import karme.EvalOpts
import karme.evaluation.enrichr.PredictionLibrary
import karme.evaluation.enrichr.EnrichrPredictionLibraryParser

case class EvaluationContext(
  references: Seq[PredictionLibrary]
)

object EvaluationContext {

  def fromOptions(opts: EvalOpts): EvaluationContext = {
    val parsedLibraries = opts.referenceFiles.map(f =>
      EnrichrPredictionLibraryParser.apply(f))
    EvaluationContext(references = parsedLibraries)
  }

}
