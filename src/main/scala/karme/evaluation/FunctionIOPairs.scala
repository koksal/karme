package karme.evaluation

import karme.synthesis.{FunctionTrees, SynthesisResult}

object FunctionIOPairs {

  def funInputOutputPairs(
    label: String, result: SynthesisResult
  ): Set[(String, String)] = {
    for (idInFunction <- namesInSynthesisResult(result)) yield {
      (idInFunction, label)
    }
  }

  def namesInSynthesisResult(r: SynthesisResult): Set[String] = {
    r.functions flatMap FunctionTrees.collectIdentifiers
  }

}
