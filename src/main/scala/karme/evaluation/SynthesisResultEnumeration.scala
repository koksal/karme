package karme.evaluation

import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.SynthesisResult
import karme.util.MathUtil

object SynthesisResultEnumeration {

  /**
    * Chooses one function expression per synthesis result and returns a
    * set of all possible combinations.
    */
  def enumerateSynthesisResultCombinations(
    labelToSynthesisResults: Map[String, Set[SynthesisResult]]
  ): Set[Map[String, FunExpr]] = {
    val labels = labelToSynthesisResults.collect{
      case (label, res) if res.nonEmpty => label
    }.toList

    val orderedResultSets = labels map { l => labelToSynthesisResults(l) }
    val product = MathUtil.cartesianProduct(orderedResultSets)

    product map { synthResults =>
      // pick an arbitrary function in each synthesis result set
      val firstFunctionInEachResult = synthResults map (r => r.functions.head)
      labels.zip(firstFunctionInEachResult).toMap
    }
  }

}
