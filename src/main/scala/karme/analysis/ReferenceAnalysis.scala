package karme.analysis

import karme.evaluation.EvaluationContext
import karme.evaluation.enrichr.EnrichrPredictionLibrary
import karme.evaluation.enrichr.PredictionEvaluator

class ReferenceAnalysis(evalContext: EvaluationContext) {

  def analyzeReferencesForClustering(
    clusteringOpt: Option[Map[String, Set[String]]]
  ): Unit = clusteringOpt match {
    case Some(clustering) => analyzeReferencesForClustering(clustering)
    case None => println("No clustering to analyze.")
  }

  def analyzeReferencesForClustering(
    clustering: Map[String, Set[String]]
  ): Unit = {
    for (reference <- evalContext.references) {
      analyzeReferenceForClustering(reference, clustering)
    }
  }

  def analyzeReferenceForClustering(
    reference: EnrichrPredictionLibrary,
    clustering: Map[String, Set[String]]
  ): Unit = {
    val referencePairs = PredictionEvaluator.referencePairs(reference)
    val clusteredNames = clustering.values.toSet.flatten

    val referencePairsInClustering = referencePairs filter {
      case (src, tgt) =>
        clusteredNames.contains(src) && clusteredNames.contains(tgt)
    }

    // What is lost by clustering / filtering?
    println(s"Total reference edges: ${referencePairs.size}")
    println(s"Reference pairs with clustered vars: " +
      s"${referencePairsInClustering.size}")

    val clusterPairToReferencePairs = groupPairsByClusterPairs(referencePairs,
      clustering)

    // How many predictions are within cluster?
    printWithinClusterPairs(clusterPairToReferencePairs)

    // How many predictions for each cluster pair?
    printNbRefPairsPerClusterPair(clusterPairToReferencePairs)
  }

  def printWithinClusterPairs(
    clusterPairToReferencePairs: Map[(String, String), Set[(String, String)]]
  ): Unit = {
    val clusterSelfPairs = clusterPairToReferencePairs.keySet filter {
      case (src, tgt) => src == tgt
    }

    val referencePairsInClusterSelfPairs = clusterSelfPairs flatMap { p =>
      clusterPairToReferencePairs(p)
    }

    println(s"# Within-cluster reference edges: " +
      s"${referencePairsInClusterSelfPairs.size}")
  }

  def printNbRefPairsPerClusterPair(
    clusterPairToReferencePairs: Map[(String, String), Set[(String, String)]]
  ): Unit = {
    for ((clusterPair, refPairs) <- clusterPairToReferencePairs) {
      println(s"# Reference edges for ${clusterPair}: ${refPairs.size}")
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

}
