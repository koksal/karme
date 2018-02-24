package karme.evaluation.synthetic

import karme.evaluation.FunExprSimilarity
import karme.evaluation.FunctionIOPairs
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.CollectionUtil

object FunSimilarityEval {

  val geneHeader = "Gene"
  val similarityHeader = "Similarity"

  val behaviorSimilarityHeaders = List(geneHeader, similarityHeader)
  val ioPairSimilarityHeaders = List(similarityHeader)

  def evaluateBehaviorSimilarity(
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

  def evaluateIOPairSimilarity(
    inferredModel: Map[String, FunExpr],
    hiddenModel: Map[String, FunExpr]
  ): Map[String, Any] = {
    val inferredModelIOPairs = FunctionIOPairs.modelInputOutputPairs(
      inferredModel)
    val hiddenModelIOPairs = FunctionIOPairs.modelInputOutputPairs(hiddenModel)

    Map(
      similarityHeader ->
        CollectionUtil.jaccardIndex(inferredModelIOPairs, hiddenModelIOPairs)
    )
  }

}
