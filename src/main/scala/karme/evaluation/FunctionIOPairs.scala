package karme.evaluation

import karme.synthesis.{FunctionTrees, SynthesisResult}

object FunctionIOPairs {

  def funInputOutputPairs(
    label: String, result: SynthesisResult
  ): Seq[(String, String)] = {
    namesInSynthesisResult(result).toSeq map { idInFunction =>
      (idInFunction, label)
    }
  }

  def namesInSynthesisResult(r: SynthesisResult): Set[String] = {
    r.functions flatMap FunctionTrees.collectIdentifiers
  }

}