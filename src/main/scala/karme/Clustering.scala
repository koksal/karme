package karme

case class Clustering(clusterToMember: Map[String, Set[String]]) {

  val memberToCluster: Map[String, String] = {
    for {
      (cluster, members) <- clusterToMember
      member <- members
    } yield {
      member -> cluster
    }
  }

  val allClusters: Set[String] = clusterToMember.keySet

  val allMembers: Set[String] = memberToCluster.keySet

}

object Clustering {

  def combineByIntersection(cs: Seq[Clustering]): Clustering = {
    val allMappings = cs.flatMap(_.clusterToMember.toSeq)

    val clusterToMappings = allMappings.groupBy(_._1)

    val combinedMappings = clusterToMappings map {
      case (cluster, mappings) =>
        val commonMembers = mappings.map(_._2).reduce(_ intersect _)
        cluster -> commonMembers
    }

    Clustering(combinedMappings)
  }

}
