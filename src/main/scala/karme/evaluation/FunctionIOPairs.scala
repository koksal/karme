package karme.evaluation

import karme.synthesis.{FunctionTrees, SynthesisResult}

object FunctionIOPairs {

  def funInputOutputPairs(
    results: Seq[Map[String, SynthesisResult]]
  ): Set[(String, String)] = {
    results.flatMap(funInputOutputPairs).toSet
  }

  def funInputOutputPairs(
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
