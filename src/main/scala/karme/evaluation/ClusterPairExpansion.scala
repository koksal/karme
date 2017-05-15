package karme.evaluation

import karme.util.MathUtil

class ClusterPairExpansion(clustering: Map[String, Set[String]]) {

  def clusterMemberPairs(
    clusterPairs: Set[(String, String)]
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
