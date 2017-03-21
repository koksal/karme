package karme.evaluation

import karme.EvalOpts
import karme.evaluation.enrichr.EnrichrPredictionLibrary
import karme.evaluation.enrichr.EnrichrPredictionLibraryParser

case class EvaluationContext(
  references: Seq[EnrichrPredictionLibrary]
)

object EvaluationContext {

  def fromOptions(opts: EvalOpts): EvaluationContext = {
    val parsedLibraries = opts.referenceFiles.map(
      EnrichrPredictionLibraryParser.apply)
    EvaluationContext(references = parsedLibraries)
  }

}
