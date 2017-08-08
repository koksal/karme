package karme

case class Clustering(clusterToMembers: Map[String, Set[String]]) {

  val memberToCluster: Map[String, String] = {
    for {
      (cluster, members) <- clusterToMembers
      member <- members
    } yield {
      member -> cluster
    }
  }

  val allClusters: Set[String] = clusterToMembers.keySet

  val allMembers: Set[String] = memberToCluster.keySet

}

object Clustering {

  def combineByIntersection(cs: Seq[Clustering]): Clustering = {
    val allMappings = cs.flatMap(_.clusterToMembers.toSeq)

    val clusterToMappings = allMappings.groupBy(_._1)

    val combinedMappings = clusterToMappings map {
      case (cluster, mappings) =>
        val commonMembers = mappings.map(_._2).reduce(_ intersect _)
        cluster -> commonMembers
    }

    Clustering(combinedMappings)
  }

}
