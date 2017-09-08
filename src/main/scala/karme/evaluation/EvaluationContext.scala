package karme.evaluation

import karme.{EvalOpts, PredictionLibrary}
import karme.evaluation.enrichr.PredictionLibraryParser

case class EvaluationContext(
  references: Seq[PredictionLibrary]
)

object EvaluationContext {

  def fromOptions(opts: EvalOpts): EvaluationContext = {
    val parsedLibraries = opts.referenceFiles.map(f =>
      PredictionLibraryParser.apply(f))
    EvaluationContext(references = parsedLibraries)
  }

}
