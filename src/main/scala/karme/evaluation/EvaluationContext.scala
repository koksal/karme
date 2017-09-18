package karme.evaluation

import karme.evaluation.reference.PredictionLibraryParser
import karme.{EvalOpts, PredictionLibrary}

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
