package karme.evaluation.enrichr

import karme.EvalOpts
import karme.evaluation.EvaluationContext
import karme.synthesis.FunctionTrees
import karme.synthesis.SynthesisResult

class PredictionEvaluator(opts: EvalOpts) {

  lazy val evalContext = EvaluationContext.fromOptions(opts)

  def compareToReferences(
    results: Seq[Map[String, SynthesisResult]],
    clustering: Option[Map[String, Set[String]]]
  ): Unit = {
    for (r <- results) {
      compareToReferences(r, clustering)
    }

    // Check against randomized data
  }

  def compareToReferences(
    result: Map[String, SynthesisResult],
    clustering: Option[Map[String, Set[String]]]
  ): Unit = {
    // For each target, gather possible sources
    val srcTgtPairs = sourceTargetPairs(result)
    println(srcTgtPairs.mkString("\n"))

    // Map cluster-level pairs to gene level
  }

  def sourceTargetPairs(
    labelToResult: Map[String, SynthesisResult]
  ): Set[(String, String)] = {
    val pairs = for {
      (label, synthResult) <- labelToResult
      identifierInFunction <- namesInSynthesisResult(synthResult)
    } yield {
      (identifierInFunction, label)
    }

    pairs.toSet
  }

  def namesInSynthesisResult(r: SynthesisResult): Set[String] = {
    r.functions flatMap FunctionTrees.collectIdentifiers
  }

}
