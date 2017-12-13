package karme.util

object MathUtil {

  def median[T](vs: Iterable[T])(implicit n: Numeric[T]): Double = {
    require(vs.nonEmpty)
    val converted = vs.map(v => n.toDouble(v))

    val sorted = converted.toList.sorted
    if (sorted.size % 2 == 0) {
      val i = sorted.size / 2 - 1
      (sorted(i) + sorted(i+1)) / 2.0
    } else {
      sorted(sorted.size / 2)
    }
  }

  def mean(vs: Iterable[Double]): Double = {
    vs.sum / vs.size
  }

  def roundTo(digits: Int)(n: Double): Double = {
    (n * (Math.pow(10.0, digits))).toInt / Math.pow(10.0, digits)
  }

  def cartesianProduct[A](ss: List[Set[A]]): Set[List[A]] = ss match {
    case s1 :: rest => {
      s1 flatMap { e =>
        cartesianProduct(rest) map { p => e :: p }
      }
    }
    case Nil => Set(Nil)
  }

  def stdev(vs: Iterable[Double]): Double = {
    val avg = mean(vs)
    val diffs = vs map (v => v - avg)
    val sqDiffs = diffs map (v => v * v)
    val normalized = sqDiffs.sum / (vs.size - 1)
    scala.math.sqrt(normalized)
  }

  def approxEquals(precision: Double)(d1: Double, d2: Double): Boolean = {
    math.abs(d1 - d2) <= precision
  }

}
