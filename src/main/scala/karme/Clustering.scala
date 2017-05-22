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

}
