package karme

object Evaluation {
  def evaluateReordering(exp: Experiment, pseudotimes: Array[Double]): Double = {
    // Linear stretching of pseudotime into real time range
    // Sum of squares between stretched time and actual time

    val actualTimes = exp.measurements.map(_.actualTime)
    val (minActualT, maxActualT) = (actualTimes.min, actualTimes.max)
    val (minPseudotime, maxPseudotime) = (pseudotimes.min, pseudotimes.max)

    val scaledActualTimes = actualTimes map (scale(_, minActualT, maxActualT))
    val scaledPseudotimes = pseudotimes map (scale(_, minPseudotime, maxPseudotime))

    meanSquaredError(scaledPseudotimes, scaledActualTimes)
  }

  private def scale(v: Double, min: Double, max: Double): Double = {
    (v - min) / (max - min)
  }

  private def meanSquaredError(xs: Seq[Double], ys: Seq[Double]): Double = {
    assert(xs.size == ys.size)
    val n = xs.size
    val sum = xs.zip(ys).map{ case (x, y) => Math.pow(x - y, 2) }.sum
    sum / n
  }
}
