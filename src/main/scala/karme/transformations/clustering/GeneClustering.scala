package karme.transformations.clustering

import karme.Experiments.{Experiment, Measurement}
import karme.synthesis.Transitions.GenericState
import karme.util.MathUtil
import karme.{Clustering, ClusteringOpts}

class GeneClustering(opts: ClusteringOpts) {

  def computeHierarchicalClustering(
    exp: Experiment[Double],
    maxNbClusters: Int
  ): Seq[Map[String, Set[String]]] = {
    val kMax = math.min(exp.names.size - 1, maxNbClusters)

    val clusterAssignments = new HclustInterface().findAllClusterings(exp, kMax)

    clusterAssignments map GeneClustering.makeClusterToNamesMap
  }

  def computeBestClustering(
    exp: Experiment[Double]
  ): Clustering = {
    computeBestClustering(exp.names, exp.valueMatrix)
  }

  def computeBestClustering(
    geneNames: Seq[String],
    valueMatrix: Seq[Seq[Double]]
  ): Clustering = {
    var minK = opts.minNbClusters
    var maxK = opts.maxNbClusters

    // fewer clusters than things to cluster
    maxK = math.min(geneNames.size - 1, maxK)

    // fewer clusters than maximum distinct values
    val uniqueDataPoints = valueMatrix.distinct
    println(s"Unique data points: ${uniqueDataPoints.size}")
    maxK = math.min(maxK, uniqueDataPoints.size - 1)

    minK = math.min(minK, maxK)

    println(s"Adjusted cluster bounds: ($minK, $maxK)")

    val clusterIndices = new NbClustInterface().cluster(valueMatrix,
      minK, maxK,
      distance = opts.clusteringDistance,
      method = opts.clusteringMethod,
      index = opts.clusteringIndex)

    val bestK = clusterIndices.toSet.size
    println(s"Best k: $bestK")

    GeneClustering.makeClustering(geneNames, clusterIndices)
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

}

object GeneClustering {

  def makeClustering(
    namesToCluster: Seq[String],
    clusterIndices: Seq[Int]
  ): Clustering = {
    val nameToClust = namesToCluster.zip(clusterIndices).toMap
    val clusterToMembers = makeClusterToNamesMap(nameToClust)
    Clustering(clusterToMembers)
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

  private def clusterName(index: Int): String = {
    s"c$index"
  }

  private def compareClusterings(
    c1: Map[String, Int], c2: Map[String, Int]
  ): Unit = {
    assert(c1.keySet == c2.keySet)
    val namesWithSameCluster = c1.keySet.count(n => c1(n) == c2(n))
    println(s"Names with same cluster: ${namesWithSameCluster}")
    println(s"Total names: ${c1.keySet.size}")
  }

}
