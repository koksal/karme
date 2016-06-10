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

  def writeExp(reporter: FileReporter, exp: AbsExperiment[_], fn: String): File = {
    val f = reporter.outFile(fn)
    FileReporter.outputTuples(f, exp.toTuples())
    f
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

  def random(seedOpt: Option[Int]): scala.util.Random = seedOpt match {
    case Some(v) => new scala.util.Random(v)
    case None => new scala.util.Random()
  }

  def time[R](block: => R): R = {
      val t0 = System.nanoTime()
      val result = block    // call-by-name
      val t1 = System.nanoTime()
      val s = (t1 - t0) / 1000000000.0
      println("Elapsed time: " + s + "s")
      result
  }

  import scala.collection.parallel._
  def parallelize[A](xs: Iterable[A], n: Int): ParIterable[A] = {
    val ps = xs.par
    ps.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(n))
    ps
  }
}
