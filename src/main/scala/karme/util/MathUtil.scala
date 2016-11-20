package karme.util

object MathUtil {

  def median(vs: Iterable[Int]): Double = {
    val sorted = vs.toList.sorted
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

  def roundTo(n: Double, digits: Int): Double = {
    (n * (Math.pow(10.0, digits))).toInt / Math.pow(10.0, digits)
  }

}
