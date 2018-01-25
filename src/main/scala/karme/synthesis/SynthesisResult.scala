package karme.synthesis

import karme.evaluation.FunExprSimilarity
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.Transition
import karme.util.MathUtil

case class SynthesisResult(
  transitions: Set[Transition],
  functions: Set[FunExpr]
)

object SynthesisResult {

  def makeCombinations(
    labelToResults: Map[String, SynthesisResult]
  ): Seq[Map[String, FunExpr]] = {
    val sortedLabels = labelToResults.keySet.toList.sorted

    val setsForProduct = sortedLabels map { label =>
      val allFuns = labelToResults(label).functions
      FunExprSimilarity.findNonRedundantSet(allFuns)
    }

    val product = MathUtil.cartesianProduct(setsForProduct)

    val combinations = product.toList map { functionCombination =>
      sortedLabels.zip(functionCombination).toMap
    }

    combinations.filter(_.nonEmpty)
  }

}
