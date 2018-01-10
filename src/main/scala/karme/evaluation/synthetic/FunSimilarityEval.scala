package karme.evaluation.synthetic

import karme.evaluation.FunExprSimilarity
import karme.synthesis.FunctionTrees.FunExpr

object FunSimilarityEval {

  val geneHeader = "Gene"
  val similarityHeader = "Similarity"

  val orderedHeaders = List(geneHeader, similarityHeader)

  def evaluateFunSimilarity(
    hiddenModel: Map[String, FunExpr],
    inferredModel: Map[String, FunExpr]
  ): Seq[Map[String, Any]] = {
    hiddenModel.keySet.toList.map { v =>
      val similarity = inferredModel.get(v) match {
        case Some(inferredFun) => {
          FunExprSimilarity.commonBehaviorRatio(hiddenModel(v), inferredFun)
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
