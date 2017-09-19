package karme.util

import java.util.concurrent.ForkJoinPool

import scala.collection.parallel.{ForkJoinTaskSupport, ParSeq}

object ParUtil {

  def withParallelism[A](nbThreads: Int, xs: Seq[A]): ParSeq[A] = {
    val parSeq = xs.par
    parSeq.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(nbThreads))
    parSeq
  }

}
