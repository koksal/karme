package karme.evaluation.synthetic

import karme.evaluation.FunExprSimilarity
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState

object FunSimilarityEval {

  val geneHeader = "Gene"
  val similarityHeader = "Similarity"

  val orderedHeaders = List(geneHeader, similarityHeader)

  def evaluateFunSimilarity(
    inferredModel: Map[String, FunExpr],
    hiddenModel: Map[String, FunExpr],
    states: Set[ConcreteBooleanState]
  ): Seq[Map[String, Any]] = {
    hiddenModel.keySet.toList.map { v =>
      val similarity = inferredModel.get(v) match {
        case Some(inferredFun) => {
          FunExprSimilarity.commonBehaviorRatio(
            hiddenModel(v),
            inferredFun,
            states
          )
        }
        case None => {
          "N/A"
        }
      }
      Map(
        geneHeader -> v,
        similarityHeader -> similarity
      )
    }
  }

}
