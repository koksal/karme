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
        val samplingT  = m.time
        val neighborMsIndices = neighGraph(i)
        val neighborST = neighborMsIndices.map(ms(_).time)
        val neighborPT = neighborMsIndices.map(prevPseudotimes(_))
        val newPT = update(samplingT, prevPseudotimes(i), neighborST, neighborPT, alpha)
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
    var graph: Map[Int,Seq[Int]] = Map.empty
    val nbComp = 1
    for (i <- 1 to nbComp) {
      graph = Util.time { closestNeighbors(ms, nbNeighbors, timeWeight) }
    }
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
    val n = ms.size
    println("N: " + n)
    val nbParTasks = 32

    (for (i1 <- Util.parallelize(0 until n, nbParTasks)) yield {
      val m1 = ms(i1)
      var indexDistances = Set[(Int, Double)]()
      for (i2 <- 0 until n) {
        if (i2 != i1) {
          val m2 = ms(i2)
          val dist = distance(m1.values, m2.values, m1.time, m2.time, timeWeight)
          indexDistances += (i2 -> dist)
        }
      }
      (i1 -> selectClosest(indexDistances.toSeq, k))
    }).seq.toMap
  }

  private def selectClosest(ds: Seq[(Int, Double)], k: Int) = {
    var kClosest = Set[(Int, Double)]()
    kClosest ++= (ds take k)
    for (d @ (i, v) <- ds drop k) {
      val max @ (maxI, maxV) = kClosest.maxBy(_._2)
      if (v < maxV) {
        kClosest -= max
        kClosest += d
      }
    }
    kClosest.map(_._1).toSeq
  }

  private def jaccardNeighbors(
    ms: IndexedSeq[CellMeasurement],
    neighborGraph: Map[Int, Seq[Int]],
    nbNeighbors: Int
  ): Map[Int, Seq[Int]] = {
    val range = ms.size
    val nbParTasks = 32
    val jacNeighbors = for (i <- Util.parallelize(0 until range, nbParTasks)) yield {
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
    var sum = 0.0
    val n = vs1.size
    for (i <- 0 until n) {
      val v1 = vs1(i)
      val v2 = vs2(i)
      val diff = v1 - v2
      sum += diff * diff
    }
    if (timeWeight > 0) {
      val tDiff = t1 - t2
      sum += timeWeight * tDiff * tDiff
    }
    math.sqrt(sum)
  }

  private def update(
    samplingTime: Double,
    pseudotime: Double, 
    neighborSamplingTimes: Seq[Double], 
    neighborPseudotimes: Seq[Double], 
    alpha: Double
  ): Double = {
    assert(neighborPseudotimes.size > 0)
    val neighborWs = neighborSamplingTimes.map(t => math.pow(math.E, - math.abs(samplingTime - t))
    val weightSum = neighborWs.sum
    val weightedNeighborSum = neighborWs.zip(neighborPseudotimes).map{
      case (w, pt) => w * pt
    }.sum
      
    val normalizedWeightedNeighborSum = weightedNeighborSum / weightSum
    val newValue = alpha * pseudotime + (1.0 - alpha) * normalizedWeightedNeighborSum
    newValue
  }

  private def approxEquals(ps1: Array[Double], ps2: Array[Double]): Boolean = {
    val epsilon = 0.0001
    ps1.zip(ps2)forall{
      case (v1, v2) => math.abs(v1 - v2) < epsilon
    }
  }
}
