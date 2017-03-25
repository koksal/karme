package karme.evaluation.enrichr

import karme.EvalOpts
import karme.evaluation.EvaluationContext
import karme.evaluation.PredictionSignificanceTest
import karme.synthesis.FunctionTrees
import karme.synthesis.SynthesisResult
import karme.util.MathUtil

class PredictionEvaluator(
  opts: EvalOpts, experimentNamesBeforeFiltering: Set[String]
) {

  lazy val evalContext = EvaluationContext.fromOptions(opts)

  def compareToReferences(
    results: Seq[Map[String, SynthesisResult]],
    clustering: Option[Map[String, Set[String]]]
  ): Unit = {
    for (reference <- evalContext.references) {
      compareToReference(results, clustering, reference)
    }
  }

  def compareToReference(
    results: Seq[Map[String, SynthesisResult]],
    clustering: Option[Map[String, Set[String]]],
    reference: EnrichrPredictionLibrary
  ): Unit = {
    for (result <- results) {
      compareToReferenceAtClusterLevel(result, clustering, reference)
    }
  }

  def compareToReferenceAtClusterLevel(
    result: Map[String, SynthesisResult],
    clustering: Option[Map[String, Set[String]]],
    reference: EnrichrPredictionLibrary
  ): Unit = {
    val predictedClusterPairs = PredictionEvaluator.sourceTargetPairsFromFunctions(result)

    // TODO generate all cluster pairs, and gather reference evidence for
    // each of them. Reference edges outside of clustered variables will be
    // discarded.

    val referenceGenePairs = PredictionEvaluator.referencePairs(reference)

    val allClusterPairs = ???
    val clusterPairToEvidenceRatio = ???

    // TODO compare predicted cluster pair ratios to all cluster pair ratios
    // Use rank sum
  }

  def printClusterPairsWithNbReferenceEdges(
    pairs: Set[(String, String)],
    clusterToReferencePairs: Map[(String, String), Set[(String, String)]]
  ): Unit = {
    val pairsSortedByReference = pairs.toSeq.sortBy(
      clusterToReferencePairs(_).size).reverse

    for (e @ (src, tgt) <- pairsSortedByReference) {
      println(List(src, tgt, clusterToReferencePairs(e).size).mkString(","))
    }
  }

  def compareToReference(
    result: Map[String, SynthesisResult],
    clustering: Option[Map[String, Set[String]]],
    reference: EnrichrPredictionLibrary
  ): Double = {
    // For each target, gather possible sources
    val unmappedPredictedPairs = PredictionEvaluator.sourceTargetPairsFromFunctions(result)

    // Map cluster-level pairs to gene level
    val mappedPredictedPairs = PredictionEvaluator.processPairsWithClustering(
      unmappedPredictedPairs, clustering)
    println(mappedPredictedPairs.mkString("\n"))

    // perform significance test between predictions and reference
    PredictionSignificanceTest.computeSignificanceForNameUniverse(
      mappedPredictedPairs, PredictionEvaluator.referencePairs(reference),
      referenceNamesInOriginalExperiment(reference))
  }

  def referenceNamesInOriginalExperiment(
    reference: EnrichrPredictionLibrary
  ): Set[String] = {
    experimentNamesBeforeFiltering.intersect(
      PredictionEvaluator.referenceNames(reference))
  }
}

object PredictionEvaluator {

  def referenceNames(
    reference: EnrichrPredictionLibrary
  ): Set[String] = {
    def predictionNames(p: EnrichrPrediction): Set[String] = p match {
      case EnrichrPrediction(term, target, _) => Set(term, target)
    }

    reference.predictions.toSet.flatMap(predictionNames)
  }

  def referencePairs(
    reference: EnrichrPredictionLibrary
  ): Set[(String, String)] = {
    reference.predictions.map{
      case EnrichrPrediction(term, target, score) => (term, target)
    }.toSet
  }

  def sourceTargetPairsFromFunctions(
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

  def groupPairsByClusterPairs(
    pairs: Set[(String, String)],
    clustering: Map[String, Set[String]]
  ): Map[(String, String), Set[(String, String)]] = {
    val memberToCluster = memberToClusterMap(clustering)

    pairs groupBy {
      case (src, tgt) => (memberToCluster(src), memberToCluster(tgt))
    }
  }

  def memberToClusterMap(
    clustering: Map[String, Set[String]]
  ): Map[String, String] = {
    clustering flatMap {
      case (k, vs) => vs map (v => (v, k))
    }
  }

  def pairsInClustering(
    pairs: Set[(String, String)],
    clustering: Map[String, Set[String]]
  ): Set[(String, String)] = {
    val namesInClustering = clustering.values.toSet.flatten

    pairs filter {
      case (src, tgt) =>
        namesInClustering.contains(src) && namesInClustering.contains(tgt)
    }
  }

}
