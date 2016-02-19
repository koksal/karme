package karme

import java.io.File

object PseudotimePropagation {
  def propagateLabels(
    exp: Experiment, 
    alpha: Double, 
    nbNeighbors: Int, 
    nbIter: Int, 
    timeWeight: Double,
    timeSplitValue: Option[Double]
  ): Array[Double] = timeSplitValue match {
    case Some(v) => {
      val (beforeSplit, afterSplit) = exp.measurements.partition(_.time <= v)
      val pseudotimes1 = propagateAux(beforeSplit, alpha, nbNeighbors, nbIter, timeWeight)
      val pseudotimes2 = propagateAux(afterSplit,  alpha, nbNeighbors, nbIter, timeWeight)
      pseudotimes1 ++ pseudotimes2
    }
    case None => {
      propagateAux(exp.measurements, alpha, nbNeighbors, nbIter, timeWeight)
    }
  }

  private def propagateAux(
    ms: IndexedSeq[CellMeasurement],
    alpha: Double,
    nbNeighbors: Int,
    nbIter: Int,
    timeWeight: Double
  ): Array[Double] = {
    println("Computing neighbor graph.")
    val directedNeighborGraph = findNeighbors(ms, nbNeighbors, timeWeight)

    var pseudotimes = Array.ofDim[Double](ms.size)
    for ((m, i) <- ms.zipWithIndex) {
      pseudotimes(i) = m.time
    }
    var newPseudotimes = pseudotimes.clone

    println("Propagating labels.")
    var iter = 0
    do {
      iter += 1
      pseudotimes = newPseudotimes.clone
      for ((m, i) <- ms.zipWithIndex) {
        val neighborPT = directedNeighborGraph(i).map(pseudotimes(_))
        val newPT = update(pseudotimes(i), neighborPT, alpha)
        newPseudotimes(i) = newPT
      }
    } while(iter < nbIter)

    newPseudotimes
  }

  def findNeighbors(
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

  def distance(
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

  def update(pseudotime: Double, neighborPseudotimes: Seq[Double], alpha: Double): Double = {
    assert(neighborPseudotimes.size > 0)
    val neighborAvg = neighborPseudotimes.sum / neighborPseudotimes.size
    val newValue = alpha * pseudotime + (1.0 - alpha) * neighborAvg
    newValue
  }

  def approxEquals(ps1: Array[Double], ps2: Array[Double]): Boolean = {
    val epsilon = 0.0001
    ps1.zip(ps2)forall{
      case (v1, v2) => math.abs(v1 - v2) < epsilon
    }
  }
}
