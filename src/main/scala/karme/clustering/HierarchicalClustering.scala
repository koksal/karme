package karme.clustering

import java.io.File

import karme.Experiments.Experiment
import karme.Experiments.Measurement
import karme.synthesis.Transitions.GenericState
import karme.util.MathUtil

object HierarchicalClustering {

  val NB_EXTRA_CLUSTERS_FOR_ELBOW_METHOD = 10

  def clusteredExperiment(
    exp: Experiment[Double], k: Int, outFolder: File, markers: Set[String]
  ): Experiment[Double] = {
    val kMax = k + NB_EXTRA_CLUSTERS_FOR_ELBOW_METHOD
    assert(exp.names.size >= kMax)

    println("Computing all cuts.")
    val allCuts = HclustInterface.computeClusterCuts(exp, kMax, outFolder)

    println("Computing withinss for each cut.")
    val withinSumSquares = allCuts map (cut => withinSumSquare(cut, exp))

    // TODO plot this as a curve in R
    println("Computed withinss:")
    for ((wss, i) <- withinSumSquares.zipWithIndex) {
      println(s"${i}: ${wss}")
    }

    // experimentFromClusterAverages(exp, clusterToNames)
    ???
  }

  private def makeClusterToNamesMap(
    nameToCluster: Map[String, Int]
  ): Map[Int, Set[String]] = {
    nameToCluster.groupBy{
      case (_, cluster) => cluster
    }.map{
      case (cluster, map) => cluster -> map.keySet
    }
  }

  private def withinSumSquare(
    cut: Map[String, Int],
    exp: Experiment[Double]
  ): Double = {
    val clusterToNames = makeClusterToNamesMap(cut)
    val clusterSums = for ((clusterIndex, names) <- clusterToNames) yield {
      val valuesPerVariable = names.toSeq map { name =>
        exp.valuesForName(name)
      }
      KmeansInterface.withinSumOfSquares(valuesPerVariable)
    }
    clusterSums.sum
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
