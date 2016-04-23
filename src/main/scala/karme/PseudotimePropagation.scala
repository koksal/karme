package karme

import java.io.File

object PseudotimePropagation {
  def propagateLabels(
    reporter: FileReporter,
    exp: Experiment, 
    alpha: Double, 
    nbNeighbors: Int, 
    nbIter: Int, 
    timeWeight: Double,
    useJaccardSimilarity: Boolean
  ): Experiment = {
    val ms = exp.measurements
    val neighGraph = neighborGraph(reporter, ms, nbNeighbors, timeWeight, useJaccardSimilarity)

    var prevPseudotimes = Array.ofDim[Double](ms.size)
    for ((m, i) <- ms.zipWithIndex) {
      prevPseudotimes(i) = m.time
    }
    var currPseudotimes = prevPseudotimes.clone

    println("Propagating labels.")
    var iter = 0
    do {
      iter += 1
      prevPseudotimes = currPseudotimes.clone
      for ((m, i) <- ms.zipWithIndex) {
        val neighborPT = neighGraph(i).map(prevPseudotimes(_))
        val newPT = update(prevPseudotimes(i), neighborPT, alpha)
        currPseudotimes(i) = newPT
      }
    } while (iter < nbIter)

    val scaledPseudotimes = RangeScaling.scalePseudotimes(
      currPseudotimes,
      exp.measurements.map(_.time).toArray
    )

    val msWithPseutotimes = ms.zip(scaledPseudotimes).map{ case (m, pt) => m.copy(pseudotime = pt )}
    exp.copy(measurements = msWithPseutotimes)
  }

  private def neighborGraph(
    reporter: FileReporter,
    ms: IndexedSeq[CellMeasurement],
    nbNeighbors: Int,
    timeWeight: Double,
    useJaccardSimilarity: Boolean
  ): Map[Int, Seq[Int]] = {
    println("Computing neighbor graph.")
    var graph = closestNeighbors(ms, nbNeighbors, timeWeight)
    // RInterface.plotNeighborGraph(reporter, ms, graph, "euclidean")
    if (useJaccardSimilarity) {
      graph = jaccardNeighbors(ms, graph, nbNeighbors)
      RInterface.plotNeighborGraph(reporter, ms, graph, "jaccard")
    }

    graph
  }

  private def closestNeighbors(
    ms: IndexedSeq[CellMeasurement], 
    k: Int, 
    timeWeight: Double
  ): Map[Int, Seq[Int]] = {
    val ds = Util.time { distances(ms, timeWeight) }
    println("Computed pairwise distances.")
    val n = ms.size
    val closestPairs = Util.time {
        for (i <- (0 until n).par) yield {
        val iDs = for (j <- 0 until n; if j != i) yield (j -> distance(i, j, ds))
        val closest = selectClosest(iDs, k)
        i -> closest
      }
    }
    println("Computed nearest neighbors.")
    closestPairs.seq.toMap
  }

  private def distance(i: Int, j: Int, ds: Array[Array[Double]]): Double = {
    if (i < j) ds(i)(j) else ds(j)(i)
  }

  private def selectClosest(ds: Seq[(Int, Double)], k: Int): Seq[Int] = {
    ds.sortBy(_._2).take(k).map(_._1)
  }

  private def distances(
    ms: IndexedSeq[CellMeasurement],
    timeWeight: Double
  ): Array[Array[Double]] = {
    val n = ms.size
    val matrix = Array.ofDim[Double](n, n)
    for {
      i <- (0 until n).par
      j <- ((i + 1) until n).par
      if i != j
    } {
      val m1 = ms(i)
      val m2 = ms(j)
      val d = distance(m1.values, m2.values, m1.time, m2.time, timeWeight)
      matrix(i)(j) = d
    }
    matrix
  }

  private def jaccardNeighbors(
    ms: IndexedSeq[CellMeasurement],
    neighborGraph: Map[Int, Seq[Int]],
    nbNeighbors: Int
  ): Map[Int, Seq[Int]] = {
    val range = ms.size
    val jacNeighbors = for (i <- (0 until range).par) yield {
      val jaccardSimilarities = (0 until range) collect {
        case j if j != i => {
          (j, jaccardSimilarity(neighborGraph, i, j))
        }
      }
      val orderedJaccardSimilarities = jaccardSimilarities.sortBy(_._2).reverse
      val nearest = orderedJaccardSimilarities.take(nbNeighbors).map(_._1)
      i -> nearest
    }
    jacNeighbors.seq.toMap
  }

  private def jaccardSimilarity(g: Map[Int, Seq[Int]], i: Int, j: Int): Double = {
    val iNeighbors = g(i).toSet
    val jNeighbors = g(j).toSet
    val intersection = iNeighbors intersect jNeighbors
    val union = iNeighbors union jNeighbors
    intersection.size.toDouble / union.size
  }

  private def distance(
    vs1: IndexedSeq[Double], 
    vs2: IndexedSeq[Double], 
    t1: Double, 
    t2: Double, 
    timeWeight: Double
  ): Double = {
    val valueSum = (vs1 zip vs2).map{ case (v1, v2) => math.pow(v1 - v2, 2) }.sum
    val sumWithTime = valueSum + timeWeight * math.pow(t1 - t2, 2)
    math.pow(sumWithTime, 0.5)
  }

  private def update(pseudotime: Double, neighborPseudotimes: Seq[Double], alpha: Double): Double = {
    assert(neighborPseudotimes.size > 0)
    val neighborAvg = neighborPseudotimes.sum / neighborPseudotimes.size
    val newValue = alpha * pseudotime + (1.0 - alpha) * neighborAvg
    newValue
  }

  private def approxEquals(ps1: Array[Double], ps2: Array[Double]): Boolean = {
    val epsilon = 0.0001
    ps1.zip(ps2)forall{
      case (v1, v2) => math.abs(v1 - v2) < epsilon
    }
  }
}
