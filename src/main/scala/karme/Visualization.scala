package karme

object Visualization {
  def plotAdjacentTimePoints(exp: Experiment, reporter: FileReporter): Unit = {
    // for all adjacent pairs of time points
    //   plot protein pair values

    val sts = exp.samplingTimes
    val stsPairs = sts.zip(sts.tail)
    val cellsBySamplingTime = exp.measurements.groupBy(_.time)

    val protsWithIndex = exp.measuredProteins.zipWithIndex
    val protPairs = protsWithIndex.zip(protsWithIndex.tail)

    for (((t1, t2), ti) <- stsPairs.zipWithIndex) {
      val t1Cells = cellsBySamplingTime(t1)
      val t2Cells = cellsBySamplingTime(t2)

      for (((p1, i1), (p2, i2)) <- protPairs) {
        val t1Rows = t1Cells map {c => 
          val x = c.values(i1)
          val y = c.values(i2)
          val group = t1
          (x, y, group)
        }
        val t2Rows = t2Cells map { c => 
          val x = c.values(i1)
          val y = c.values(i2)
          val group = t2
          (x, y, group)
        }
        val allRows = t1Rows ++ t2Rows
        val f = reporter.outFile(s"$p1-$p2-time-pair-$ti.pdf")
        RInterface.groupedScatterPlot(f, allRows)
      }
    }
  }
}
