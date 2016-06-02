package karme

object CellReordering {
  def computePseudotimes(exp: Experiment): Experiment = {
    val meanValuesPerTime: Map[Double, IndexedSeq[Double]] = computeMeanCells(exp)

    // place each cell between two mean vectors.
    // find two closest mean cells
    // place between the two
    // pseudotime will be the weighted average of mean cell time stamps
    // question: can we detect if cell is not on the segment between the two?

    val msWithPt = exp.measurements.map { m =>
      val pt = computeCellPseudotime(m, meanValuesPerTime)
      m.copy(pseudotime = pt)
    }

    exp.copy(measurements = msWithPt)
  }

  private def computeMeanCells(exp: Experiment): Map[Double, IndexedSeq[Double]] = {
    val cellsBySamplingTime = exp.measurements.groupBy(_.time)
    val means = cellsBySamplingTime.map{ case (t, ms) =>
      t -> Transformations.meanValues(ms)
    }
    means
  }

  private def computeCellPseudotime(
    m: CellMeasurement, 
    means: Map[Double, IndexedSeq[Double]]
  ): Double = {
    val allowedSamplingTimes = means.keySet.toList.sortBy(samplingTime => math.abs(samplingTime - m.time)).take(3)
    val distances = allowedSamplingTimes map { t =>
      val mvs = means(t)
      t -> distance(m.values, mvs)
    }

    val sortedDistances = distances.toList.sortBy(_._2)
    val List((t1, d1), (t2, d2)) = sortedDistances take 2

    // place cell between t1 and t2
    val scaleF = RangeScaling.scalingFun(0, d1 + d2, t1, t2)
    scaleF(d1)
  }

  private def distance(vs1: IndexedSeq[Double], vs2: IndexedSeq[Double]): Double = {
    val squares = vs1.zip(vs2).map{ case (v1, v2) => 
      val diff = v1 - v2
      diff * diff
    }
    val squareSum = squares.sum
    math.sqrt(squareSum)
  }
}
