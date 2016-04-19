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
    nbNeighbors: Int, 
    timeWeight: Double
  ): Map[Int, Seq[Int]] = {
    val pairs = for ((m1, i1) <- ms.par.zipWithIndex) yield {
      val indexDistances = ms.zipWithIndex collect { 
        case (m2, i2) if i2 != i1 => (i2, distance(m1.values, m2.values, m1.time, m2.time, timeWeight)) 
      }
      val orderedIndexDistances = indexDistances.sortBy(_._2)
      val nearestIndices = orderedIndexDistances.take(nbNeighbors).map(_._1)
      i1 -> nearestIndices
    }

    pairs.seq.toMap
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
