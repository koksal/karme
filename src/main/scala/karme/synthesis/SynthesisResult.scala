package karme.synthesis

import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.Transition
import karme.util.MathUtil

case class SynthesisResult(
  transitions: Set[Transition],
  functions: Set[FunExpr]
) {

  def simplify: SynthesisResult = {
    this.copy(functions = functions map FunctionTrees.simplify)
  }

  def canonicalize: SynthesisResult = {
    this.copy(functions = functions map FunctionTrees.canonicalize)
  }

}

object SynthesisResult {

  def makeCombinations(
    labelToResults: Map[String, Set[SynthesisResult]]
  ): Seq[Map[String, FunExpr]] = {
    val labelsWithResults = labelToResults.filter(_._2.nonEmpty).keySet.toList
    val setsForProduct = labelsWithResults map { label =>
      labelToResults(label)
        .map(_.simplify)
        .map(_.canonicalize)
        .flatMap(_.functions)
    }

    val product = MathUtil.cartesianProduct(setsForProduct)

    product.toList map { functionCombination =>
      labelsWithResults.zip(functionCombination).toMap
    }
  }

}
