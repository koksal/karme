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

}
