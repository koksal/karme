package karme.evaluation.enrichr

import karme.EvalOpts
import karme.evaluation.EvaluationContext
import karme.evaluation.PredictionSignificanceTest
import karme.synthesis.FunctionTrees
import karme.synthesis.SynthesisResult
import karme.util.MathUtil

class PredictionEvaluator(opts: EvalOpts, allLabels: Set[String]) {

  lazy val evalContext = EvaluationContext.fromOptions(opts)

  def compareToReferences(
    results: Seq[Map[String, SynthesisResult]],
    clustering: Option[Map[String, Set[String]]]
  ): Unit = {
    for (reference <- evalContext.references) {
      compareToReference(results, clustering, reference)
    }

    // TODO Check against randomized data
  }

  def compareToReference(
    results: Seq[Map[String, SynthesisResult]],
    clustering: Option[Map[String, Set[String]]],
    reference: EnrichrPredictionLibrary
  ): Unit = {
    // TODO alternative: aggregating all results for comparison
    for (result <- results) {
      val score = compareToReference(result, clustering, reference)
      println(s"Score: $score")
    }
  }

  def compareToReference(
    result: Map[String, SynthesisResult],
    clustering: Option[Map[String, Set[String]]],
    reference: EnrichrPredictionLibrary
  ): Double = {
    // For each target, gather possible sources
    val unmappedPredictedPairs = PredictionEvaluator.sourceTargetPairs(result)

    // Map cluster-level pairs to gene level
    val mappedPredictedPairs = PredictionEvaluator.processPairsWithClustering(
      unmappedPredictedPairs, clustering)
    println(mappedPredictedPairs.mkString("\n"))

    // perform significance test between predictions and reference
    PredictionSignificanceTest.computeSignificanceWithoutSelfEdges(
      mappedPredictedPairs, PredictionEvaluator.referencePairs(reference),
      getNameUniverse(clustering))
  }

  def getNameUniverse(
    clusteringOpt: Option[Map[String, Set[String]]]
  ): Set[String] = clusteringOpt match {
    case Some(clustering) => clustering.values.toSet.flatten
    case None => allLabels
  }
}

object PredictionEvaluator {

  def referencePairs(
    reference: EnrichrPredictionLibrary
  ): Set[(String, String)] = {
    reference.predictions.map{
      case EnrichrPrediction(term, target, score) => (term, target)
    }.toSet
  }

  def sourceTargetPairs(
    labelToResult: Map[String, SynthesisResult]
  ): Set[(String, String)] = {
    val pairs = for {
      (label, synthResult) <- labelToResult
      identifierInFunction <- namesInSynthesisResult(synthResult)
    } yield {
      (identifierInFunction, label)
    }

    pairs.toSet
  }

  def namesInSynthesisResult(r: SynthesisResult): Set[String] = {
    r.functions flatMap FunctionTrees.collectIdentifiers
  }

  def processPairsWithClustering(
    pairs: Set[(String, String)],
    clusteringOpt: Option[Map[String, Set[String]]]
  ): Set[(String, String)] = clusteringOpt match {
    case Some(clustering) => clusterMemberPairs(pairs, clustering)
    case None => pairs
  }

  def clusterMemberPairs(
    clusterPairs: Set[(String, String)],
    clustering: Map[String, Set[String]]
  ): Set[(String, String)] = {
    clusterPairs flatMap {
      case (source, target) =>
        betweenClusterPairs(clustering(source), clustering(target))
    }
  }

  def betweenClusterPairs(
    sourceCluster: Set[String], targetCluster: Set[String]
  ): Set[(String, String)] = {
    val lists = MathUtil.cartesianProduct(List(sourceCluster, targetCluster))

    // convert two-element lists to pairs
    lists map {
      case List(src, tgt) => (src, tgt)
    }
  }

}
