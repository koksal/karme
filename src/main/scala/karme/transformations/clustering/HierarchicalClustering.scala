package karme.transformations.clustering

import karme.ClusteringOpts
import karme.Experiments.Experiment
import karme.Experiments.Measurement
import karme.synthesis.Transitions.GenericState
import karme.util.MathUtil

object HierarchicalClustering {

  def clusterVariables(
    exp: Experiment[Double],
    annotationVars: Set[String],
    opts: ClusteringOpts
  ): Map[String, Set[String]] = {
    val adjustedMaxNbClust = math.min(exp.names.size - 1, opts.maxNbClusters)
    val adjustedMinNbClust = math.min(opts.minNbClusters, adjustedMaxNbClust)
    if (adjustedMaxNbClust != opts.maxNbClusters) {
      println(s"Setting boundaries for k to ($adjustedMinNbClust, " +
        s"$adjustedMaxNbClust).")
    }

    // TODO this is temporary, keeps the last clustering only
    var clustering: Map[String, Int] = Map.empty
    val indices = List("kl")
    val methods = List("ward.D2")
    for {
      index <- indices
      method <- methods
    } {
      val nbClustPartition = new NbClustInterface(exp.valueMatrix,
        adjustedMinNbClust, adjustedMaxNbClust, method = method,
        index = index).run()
      clustering = exp.names.zip(nbClustPartition).toMap
      val bestK = nbClustPartition.toSet.size
      println(s"Optimal nb. clusters for ($index, $method): $bestK")
    }

    // print membership for annotation variables
    for (annotationVar <- annotationVars.toSeq.sorted) {
      clustering.get(annotationVar) match {
        case Some(c) => {
          println(s"$annotationVar is in cluster $c.")
        }
        case None => {
          println(s"$annotationVar is not in any cluster.")
        }
      }
    }

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

  private def withinSumSquare(
    cut: Map[String, Int],
    exp: Experiment[Double]
  ): Double = {
    val clusterToNames = makeClusterToNamesMap(cut)
    val clusterSums = for ((clusterIndex, names) <- clusterToNames) yield {
      val valuesPerVariable = names.toSeq map { name =>
        exp.valuesForName(name)
      }
      new WithinssInterface(valuesPerVariable).run()
    }
    clusterSums.sum
  }

  def experimentFromClusterAverages(
    exp: Experiment[Double],
    clusterToNames: Map[String, Set[String]],
    annotationVars: Set[String]
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
    s"cluster_$index"
  }

  private def annotatedClusterName(
    index: Int,
    clusterToNames: Map[Int, Set[String]],
    annotationVars: Set[String]
  ): String = {
    val annotationsInCluster = annotationVars.intersect(clusterToNames(index))
    val annotationStr = if (annotationsInCluster.isEmpty) {
      ""
    } else {
      "_" + annotationsInCluster.mkString(",")
    }
    s"c_${index}${annotationStr}"
  }

}
