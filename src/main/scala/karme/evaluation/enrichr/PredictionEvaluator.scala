package karme.evaluation.enrichr

import karme.EvalOpts
import karme.evaluation.EvaluationContext
import karme.synthesis.FunctionTrees
import karme.synthesis.SynthesisResult
import karme.util.MathUtil

class PredictionEvaluator(opts: EvalOpts) {

  lazy val evalContext = EvaluationContext.fromOptions(opts)

  def compareToReferences(
    results: Seq[Map[String, SynthesisResult]],
    clustering: Option[Map[String, Set[String]]]
  ): Unit = {
    for (reference <- evalContext.references) {
      compareToReference(results, clustering, reference)
    }

    // Check against randomized data
  }

  def compareToReference(
    results: Seq[Map[String, SynthesisResult]],
    clustering: Option[Map[String, Set[String]]],
    reference: EnrichrPredictionLibrary
  ): Unit = {
    // TODO alternative: aggregating all results for comparison
    for (result <- results) {
      compareToReference(result, clustering, reference)
    }
  }

  def compareToReference(
    result: Map[String, SynthesisResult],
    clustering: Option[Map[String, Set[String]]],
    reference: EnrichrPredictionLibrary
  ): Unit = {
    // For each target, gather possible sources
    val unmappedPairs = sourceTargetPairs(result)

    // Map cluster-level pairs to gene level
    val mappedPairs = processPairsWithClustering(unmappedPairs, clustering)
    println(mappedPairs.mkString("\n"))
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
