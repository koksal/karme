package karme

import java.io._
import scala.io._

object Util {
  def writeToFile(file: File, content: String, append: Boolean = false) {
    // create parent if nonexistent
    val parent = file.getParentFile
    if (parent != null) parent.mkdirs

    val fname = file.getAbsolutePath
    val out = new PrintWriter(new BufferedWriter(new FileWriter(fname, append)))
    try{ out.print( content ) }
    finally{ out.close }
  }

  def mean(vs: Iterable[Double]): Double = vs.sum / vs.size

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
}
