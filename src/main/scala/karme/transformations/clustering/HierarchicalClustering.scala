package karme.transformations.clustering

import karme.ClusteringOpts
import karme.Experiments.Experiment
import karme.Experiments.Measurement
import karme.synthesis.Transitions.GenericState
import karme.util.MathUtil

object HierarchicalClustering {

  def computeHierarchicalClustering(
    exp: Experiment[Double],
    maxNbClusters: Int
  ): Seq[Map[String, Set[String]]] = {
    val kMax = math.min(exp.names.size - 1, maxNbClusters)

    val clusterAssignments = new HclustInterface(exp, kMax).run()

    clusterAssignments map makeClusterToNamesMap
  }

  def computeGapStatClustering(
    exp: Experiment[Double],
    opts: ClusteringOpts
  ): Map[String, Set[String]] = {
    val clusterIndices = new ClusGapInterface(exp.valueMatrix,
      opts.maxNbClusters).run()

    val clustering = exp.names.zip(clusterIndices).toMap

    val bestK = clusterIndices.toSet.size
    println(s"Best k for gap statistic: $bestK")

    makeClusterToNamesMap(clustering)
  }

  def computeBestClustering(
    exp: Experiment[Double],
    opts: ClusteringOpts
  ): Map[String, Set[String]] = {
    val adjustedMaxNbClust = math.min(exp.names.size - 1, opts.maxNbClusters)
    val adjustedMinNbClust = math.min(opts.minNbClusters, adjustedMaxNbClust)

    if (adjustedMaxNbClust != opts.maxNbClusters) {
      println(s"Setting boundaries for k to ($adjustedMinNbClust, " +
        s"$adjustedMaxNbClust).")
    }

    val index = "gap"
    val method = "ward.D2"

    val clusterIndices = new NbClustInterface(exp.valueMatrix,
      adjustedMinNbClust, adjustedMaxNbClust, method = method,
      index = index).run()
    val clustering = exp.names.zip(clusterIndices).toMap

    val bestK = clusterIndices.toSet.size
    println(s"Best k: $bestK")

    makeClusterToNamesMap(clustering)
  }

  private def compareClusterings(
    c1: Map[String, Int], c2: Map[String, Int]
  ): Unit = {
    assert(c1.keySet == c2.keySet)
    val namesWithSameCluster = c1.keySet.count(n => c1(n) == c2(n))
    println(s"Names with same cluster: ${namesWithSameCluster}")
    println(s"Total names: ${c1.keySet.size}")
  }

  private def makeClusterToNamesMap(
    nameToClusterIndex: Map[String, Int]
  ): Map[String, Set[String]] = {
    nameToClusterIndex.groupBy{
      case (_, i) => i
    }.map{
      case (i, map) => clusterName(i) -> map.keySet
    }
  }

  def experimentFromClusterAverages(
    exp: Experiment[Double],
    clusterToNames: Map[String, Set[String]]
  ): Experiment[Double] = {
    // for each measurement, compute cluster averages
    val clusterMs = exp.measurements map { m =>
      val clusterToMeanValue = clusterToNames.map{
        case (clusterName, names) => {
          val values = names.map(n => m.state.value(n))
          clusterName -> MathUtil.mean(values)
        }
      }
      Measurement(m.id, GenericState(clusterToMeanValue))
    }

    Experiment(clusterMs)
  }

  private def clusterName(index: Int): String = {
    s"c$index"
  }

}
