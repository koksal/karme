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
    k: Int,
    annotationVars: Set[String],
    elbowMethod: Boolean,
    outFolder: File
  ): Map[Int, Set[String]] = {
    assert(exp.names.size >= k)

    println("Computing all cuts.")
    val allCuts = HclustInterface.computeClusterCuts(exp, k, outFolder)

    if (elbowMethod) {
      println("Computing withinss for each cut.")
      val withinSumSquares = allCuts map (cut => withinSumSquare(cut, exp))

      val points = withinSumSquares.zipWithIndex map {
        case (ss, i) => (i + 1, ss, "withinss")
      }
      ScatterPlot.plot(
        points,
        new File(outFolder, "withinSumSquares-vs-nbClusters.pdf")
      )
    }

    val kCut = allCuts.last

    // print membership for annotation variables
    for (annotationVar <- annotationVars.toSeq.sorted) {
      kCut.get(annotationVar) match {
        case Some(c) => {
          println(s"$annotationVar is in cluster $c.")
        }
        case None => {
          println(s"$annotationVar is not in any cluster.")
        }
      }
    }

    makeClusterToNamesMap(kCut)
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
