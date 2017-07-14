package karme.analysis

import java.io.File

import karme.Clustering
import karme.store.ClusteringStore

import scala.io.Source

object ClusteringAnalysis {

  def main(args: Array[String]): Unit = {
    val names = Source.fromFile(args(0)).getLines().toSet

    val runFolder = new File(args(1))
    val clusterToMember = new ClusteringStore(runFolder).read

    printMembership(names, Clustering(clusterToMember))
  }

  def printMembership(names: Set[String], clustering: Clustering): Unit = {
    val clusterToNames = names.groupBy(n => clustering.memberToCluster.get(n))

    for ((cluster, namesInCluster) <- clusterToNames) {
      println(s"Cluster: ${cluster.getOrElse("None")}")
      println(namesInCluster.toList.sorted.mkString("\n"))
      println()
    }
  }

}
