package karme.clustering

import karme.Experiments.Experiment
import karme.Experiments.Measurement
import karme.synthesis.Transitions.GenericState
import karme.util.MathUtil

object HierarchicalClustering {

  def clusteredExperiment(
    exp: Experiment[Double], k: Int
  ): Experiment[Double] = {
    val nameToCluster = HclustInterface.clusterAndCutree(exp, k)
    val clusterToNames = makeClusterToNameMap(nameToCluster)
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
