package karme

import java.io.File

object PseudotimePropagation {
  def propagateLabels(
    exp: Experiment, 
    alpha: Double, 
    nbNeighbors: Int, 
    nbIter: Int, 
    timeSplitValue: Option[Double]
  ): Array[Double] = timeSplitValue match {
    case Some(v) => {
      val (beforeSplit, afterSplit) = exp.measurements.partition(_.time <= v)
      val pseudotimes1 = propagateAux(beforeSplit, alpha, nbNeighbors, nbIter)
      val pseudotimes2 = propagateAux(afterSplit,  alpha, nbNeighbors, nbIter)
      pseudotimes1 ++ pseudotimes2
    }
    case None => {
      propagateAux(exp.measurements, alpha, nbNeighbors, nbIter)
    }
  }

  private def propagateAux(
    ms: IndexedSeq[CellMeasurement],
    alpha: Double,
    nbNeighbors: Int,
    nbIter: Int
  ): Array[Double] = {
    println("Computing neighbor graph.")
    val directedNeighborGraph = findNeighbors(ms, nbNeighbors)

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

  def findNeighbors(ms: IndexedSeq[CellMeasurement], nbNeighbors: Int): Map[Int, Seq[Int]] = {
    val pairs = for ((m1, i1) <- ms.par.zipWithIndex) yield {
      val indexDistances = ms.zipWithIndex collect { 
        case (m2, i2) if i2 != i1 => (i2, distance(m1.values, m2.values)) 
      }
      val orderedIndexDistances = indexDistances.sortBy(_._2)
      val nearestIndices = orderedIndexDistances.take(nbNeighbors).map(_._1)
      i1 -> nearestIndices
    }

    pairs.seq.toMap
  }

  def distance(vs1: IndexedSeq[Double], vs2: IndexedSeq[Double]): Double = {
    val sum = (vs1 zip vs2).map{ case (v1, v2) => math.pow(v1 - v2, 2) }.sum
    math.pow(sum, 0.5)
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
