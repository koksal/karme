package karme.evaluation

import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.{FunctionTrees, SynthesisResult}

object FunctionIOPairs {

  def funInputOutputPairs(
    label: String, result: SynthesisResult
  ): Seq[(String, String)] = {
    namesInSynthesisResult(result).toSeq map { idInFunction =>
      (idInFunction, label)
    }
  }

  def funInputOutputPairs(
    label: String, expr: FunExpr
  ): Set[(String, String)] = {
    FunctionTrees.collectIdentifiers(expr) map { inputName =>
      (inputName, label)
    }
  }

  def modelInputOutputPairs(
    model: Map[String, FunExpr]
  ): Set[(String, String)] = {
    model.flatMap{
      case (id, fe) => funInputOutputPairs(id, fe)
    }.toSet
  }

  def namesInSynthesisResult(r: SynthesisResult): Set[String] = {
    r.functions flatMap FunctionTrees.collectIdentifiers
  }

}
