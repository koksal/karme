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
    labelToResults: Map[String, Set[SynthesisResult]]
  ): Seq[Map[String, FunExpr]] = {
    val labelsWithResults = labelToResults.filter(_._2.nonEmpty).keySet.toList
    val setsForProduct = labelsWithResults map { label =>
      val allFuns = labelToResults(label).flatMap(_.functions)
      FunExprSimilarity.findNonRedundantSet(allFuns)
    }

    val product = MathUtil.cartesianProduct(setsForProduct)

    product.toList map { functionCombination =>
      labelsWithResults.zip(functionCombination).toMap
    }
  }

}
