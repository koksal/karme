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
    goalK: Int,
    annotationVars: Set[String],
    elbowMethod: Boolean,
    outFolder: File
  ): Map[Int, Set[String]] = {
    if (exp.names.size < goalK) {
      println(s"There are fewer dimensions in the experiment " +
        s"(${exp.names.size}) than the maximum number of clusters ($goalK).")
    }
    val actualK = math.min(exp.names.size - 1, goalK)

    println("Computing all cuts.")
    val allCuts = new HclustInterface(exp, actualK, outFolder).run()

    val kCut = if (elbowMethod) {
      val nbClustPartition = new NbClustInterface(exp.valueMatrix, 2, actualK,
        "gap").run()
      val nbClustMap = exp.names.zip(nbClustPartition).toMap
      val bestK = nbClustPartition.toSet.size
      println(s"Nb clusters according to NbClust: $bestK")

      // TODO compare result from nbclust to bestK-cut
      val hclustCut = allCuts(bestK - 1)
      compareClusterings(hclustCut, nbClustMap)
      hclustCut
    } else {
      allCuts.last
    }

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

  private def compareClusterings(
    c1: Map[String, Int], c2: Map[String, Int]
  ): Unit = {
    assert(c1.keySet == c2.keySet)
    val namesWithSameCluster = c1.keySet.count(n => c1(n) == c2(n))
    println(s"Names with same cluster: ${namesWithSameCluster}")
    println(s"Total names: ${c1.keySet.size}")
  }

  private def bestKByChIndex(is: Seq[Double]): Int = {
    val max = is.max
    val i = is.indexWhere(_ == max)
    // the ch indices start from k = 2
    i + 2
  }

  private def chIndices(
    withinssSeq: Seq[Double], betweenssSeq: Seq[Double], n: Int
  ): Seq[Double] = {
    // the index is not defined for 1, i.e. the first within/between sums
    withinssSeq.tail.zip(betweenssSeq.tail).zipWithIndex map {
      case ((wss, bss), i) =>
        val k = i + 2
        chIndex(wss, bss, k, n)
    }
  }

  private def chIndex(
    withinss: Double, betweenss: Double, k: Int, n: Int
  ): Double = {
    (betweenss / (k - 1)) / (withinss / (n - k))
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
