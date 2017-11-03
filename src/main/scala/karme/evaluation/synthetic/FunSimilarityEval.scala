package karme.evaluation.synthetic

import karme.evaluation.FunExprSimilarity
import karme.synthesis.FunctionTrees.FunExpr

object FunSimilarityEval {

  def evaluateFunSimilarity(
    hiddenModel: Map[String, FunExpr],
    inferredModel: Map[String, FunExpr]
  ): Map[String, Any] = {
    hiddenModel.keySet.map { v =>
      val similarity = inferredModel.get(v) match {
        case Some(inferredFun) => {
          FunExprSimilarity.commonBehaviorRatio(hiddenModel(v), inferredFun)
        }
        case None => {
          "N/A"
        }
      }
      v -> similarity
    }.toMap
  }

}
