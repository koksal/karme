package karme.util

import org.ddahl.rscala.RClient

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
}
