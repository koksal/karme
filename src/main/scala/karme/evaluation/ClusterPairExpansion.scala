package karme.evaluation

import karme.util.MathUtil

class ClusterPairExpansion(clustering: Map[String, Set[String]]) {

  def clusterMemberPairs(
    clusterPairs: Seq[(String, String)]
  ): Seq[(String, String)] = {
    clusterPairs flatMap {
      case (source, target) =>
        betweenClusterPairs(clustering(source), clustering(target))
    }
  }

  def betweenClusterPairs(
    sourceCluster: Set[String], targetCluster: Set[String]
  ): Seq[(String, String)] = {
    val lists = MathUtil.cartesianProduct(List(sourceCluster, targetCluster))

    // convert two-element lists to pairs
    lists.toList map {
      case List(src, tgt) => (src, tgt)
    }
  }

}
