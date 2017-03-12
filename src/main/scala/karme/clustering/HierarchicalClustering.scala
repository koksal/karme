package karme.clustering

import java.io.File

import karme.Experiments.Experiment
import karme.Experiments.Measurement
import karme.synthesis.Transitions.GenericState
import karme.util.MathUtil
import karme.visualization.ScatterPlot

object HierarchicalClustering {

  def clusterVariables(
    exp: Experiment[Double],
    annotationVars: Set[String],
    minNbClust: Int,
    maxNbClust: Int,
    outFolder: File
  ): Map[Int, Set[String]] = {
    val adjustedMaxNbClust = math.min(exp.names.size - 1, maxNbClust)
    if (adjustedMaxNbClust != maxNbClust) {
      println(s"Setting max k to $adjustedMaxNbClust instead of $maxNbClust.")
    }

    // TODO this is temporary, keeps the last clustering only
    var clustering: Map[String, Int] = Map.empty
    val indices = List("kl", "ch", "gap")
    val methods = List("ward.D2", "complete", "average")
    for {
      index <- indices
      method <- methods
    } {
      val nbClustPartition = new NbClustInterface(exp.valueMatrix, minNbClust,
        adjustedMaxNbClust, method = method, index = index).run()
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
      new WithinssInterface(valuesPerVariable).run()
    }
    clusterSums.sum
  }

  def experimentFromClusterAverages(
    exp: Experiment[Double],
    clusterToNames: Map[Int, Set[String]],
    annotationVars: Set[String]
  ): Experiment[Double] = {
    // for each measurement, compute cluster averages
    val clusterMs = exp.measurements map { m =>
      val clusterToMeanValue = clusterToNames.map{
        case (clusterIndex, names) => {
          val values = names.map(n => m.state.value(n))
          val cname = clusterName(clusterIndex, clusterToNames, annotationVars)
          cname -> MathUtil.mean(values)
        }
      }
      Measurement(m.id, GenericState(clusterToMeanValue))
    }

    Experiment(clusterMs)
  }

  private def clusterName(
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
