package karme.clustering

import java.io.File

import karme.Experiments.Experiment
import karme.Experiments.Measurement
import karme.synthesis.Transitions.GenericState
import karme.util.MathUtil

object HierarchicalClustering {

  def clusteredExperiment(
    exp: Experiment[Double], k: Int, outFolder: File, markers: Set[String]
  ): Experiment[Double] = {
    assert(exp.names.size >= k)

    val nameToCluster = HclustInterface.computeOptimalClustering(exp, k, outFolder)
    val clusterToNames = makeClusterToNameMap(nameToCluster)

    for (marker <- markers.toList.sorted) {
      println(s"${marker} in cluster ${nameToCluster.get(marker)}")
    }
    experimentFromClusterAverages(exp, clusterToNames)
  }

  private def makeClusterToNameMap(
    nameToCluster: Map[String, Int]
  ): Map[Int, Set[String]] = {
    nameToCluster.groupBy{
      case (_, cluster) => cluster
    }.map{
      case (cluster, map) => cluster -> map.keySet
    }
  }

  private def experimentFromClusterAverages(
    exp: Experiment[Double], clusterToNames: Map[Int, Set[String]]
  ): Experiment[Double] = {
    // for each measurement, compute cluster averages
    val clusterMs = exp.measurements map { m =>
      val clusterToMeanValue = clusterToNames.map{
        case (cluster, names) => {
          val values = names.map(n => m.state.value(n))
          s"cluster_${cluster}" -> MathUtil.mean(values)
        }
      }
      Measurement(m.id, GenericState(clusterToMeanValue))
    }

    Experiment(clusterMs)
  }

}
