package karme.evaluation.enrichr

import karme.EvalOpts
import karme.Reporter
import karme.evaluation.{ClusterPairExpansion, EvaluationContext, FunctionIOPairs}
import karme.synthesis.SynthesisResult
import karme.transformations.RankSumTest

class PredictionEvaluator(
  opts: EvalOpts,
  experimentNamesBeforeFiltering: Set[String],
  clustering: Map[String, Set[String]],
  reporter: Reporter
) {

  lazy val evalContext = EvaluationContext.fromOptions(opts)

  lazy val memberToCluster: Map[String, String] = {
    PredictionEvaluator.memberToClusterMap(clustering)
  }

  def computeReferencePValues(
    results: Seq[Map[String, SynthesisResult]]
  ): Seq[(EnrichrPredictionLibrary, Double)] = {
    for (reference <- evalContext.references) yield {
      reference -> computeReferencePValues(results, reference)
    }
  }

  def computeReferencePValues(
    results: Seq[Map[String, SynthesisResult]],
    reference: EnrichrPredictionLibrary
  ): Double = {
    val predictedClusterPairs = ???

    val refPairs = PredictionEvaluator.referencePairs(reference)

    var clusterPairToEvidenceRatio: Map[(String, String), Double] = Map.empty
    for {
      srcClust <- clustering.keySet
      tgtClust <- clustering.keySet
    } {
      clusterPairToEvidenceRatio += (srcClust, tgtClust) ->
        pairEvidenceRatio(refPairs, srcClust, tgtClust)
    }

    testPredictionSignificance(clusterPairToEvidenceRatio,
      predictedClusterPairs)
  }

  def testPredictionSignificance(
    clusterPairToEvidenceRatio: Map[(String, String), Double],
    predictedClusterPairs: Set[(String, String)]
  ): Double = {
    val allRatios = clusterPairToEvidenceRatio.values.toSeq
    val predictionRatios = predictedClusterPairs.toSeq map { pair =>
      clusterPairToEvidenceRatio(pair)
    }

    reporter.debug("Invoking rank sum:")
    reporter.debug(s"# predictions: ${predictionRatios.size}")
    reporter.debug(s"# total ratio: ${allRatios.size}")
    reporter.debug(predictionRatios.sorted.reverse)
    reporter.debug(allRatios.sorted.reverse)

    val res = new RankSumTest().test(predictionRatios, allRatios)
    res.pValue
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

    // Map cluster-level pairs to gene level
    clustering match {
      case Some(c) => {
      }
      case None =>
    }

    // perform significance test between predictions and reference
    ???
  }

  def referenceNamesInOriginalExperiment(
    reference: EnrichrPredictionLibrary
  ): Set[String] = {
    experimentNamesBeforeFiltering.intersect(
      PredictionEvaluator.referenceNames(reference))
  }

  def pairEvidenceRatio(
    referencePairs: Set[(String, String)],
    srcCluster: String,
    tgtCluster: String
  ): Double = {
    val matchingPairs = referencePairs filter {
      case (src, tgt) => {
        (memberToCluster.get(src), memberToCluster.get(tgt)) match {
          case (Some(srcMemberClust), Some(tgtMemberClust)) => {
            srcMemberClust == srcCluster && tgtMemberClust == tgtCluster
          }
          case _ => false
        }
      }
    }
    val clusterProductSize = clustering(srcCluster).size *
      clustering(tgtCluster).size
    matchingPairs.size.toDouble / clusterProductSize
  }

}

object PredictionEvaluator {

  def referenceNames(
    reference: EnrichrPredictionLibrary
  ): Set[String] = {
    def predictionNames(p: ReferencePrediction): Set[String] = p match {
      case ReferencePrediction(term, target, _) => Set(term, target)
    }

    reference.predictions.toSet.flatMap(predictionNames)
  }

  def referencePairs(
    reference: EnrichrPredictionLibrary
  ): Set[(String, String)] = {
    reference.predictions.map{
      case ReferencePrediction(term, target, score) => (term, target)
    }.toSet
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
