package karme.emd

import karme._

object MultivariateEMD {
  def plotMEMD(reporter: FileReporter, exp: Experiment, memd: Seq[Seq[Seq[Double]]]): Unit = {
    val orderedMs = exp.measurements.sortBy(_.pseudotime)
    val xs = orderedMs.map(_.pseudotime)
    for ((p, i) <- exp.measuredProteins.zipWithIndex) {
      val emd = memd(i)
      println(s"Components for $p: ${emd.size}")
      for ((vs, j) <- emd.zipWithIndex) {
        val n = s"$p-mode-$j.pdf"
        val f = reporter.outFile(n)
        RInterface.scatterPlot(f, Transformations.sampleSequence(xs zip vs))
      }
    }
  }
}
