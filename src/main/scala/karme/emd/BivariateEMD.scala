package karme.emd

import karme._
import java.io.File

object BivariateEMD {
  def plot(reporter: FileReporter, exp: Experiment) = {
    def pairs(n: Int) = {
      val res = for (i <- 0 until n) yield {
        for (j <- i + 1 until n; if i != j) yield {
          (i, j)
        }
      }
      res.flatten
    }

    val orderedMs = exp.measurements.sortBy(_.pseudotime)
    val orderedTs = orderedMs.map(_.pseudotime)
    val n = exp.measuredProteins.size
    for ((i, j) <- Util.parallelize(pairs(n), 32)) {
      val p1 = exp.measuredProteins(i)
      val p2 = exp.measuredProteins(j)
      println(s"Running BEMD for $p1 and $p2")

      val xs = orderedMs.map(_.values(i))
      val ys = orderedMs.map(_.values(j))
      
      val (xIMFs, yIMFs) = MatlabInterface.bemd(xs, ys)

      val folder = reporter.outFile(s"BEMD-$p1-$p2")
      folder.mkdirs

      val nbIMFtoPlot = 2
      for ((xIMF, imfIndex) <- xIMFs.zipWithIndex.drop(xIMFs.size - nbIMFtoPlot)) {
        val n = s"$p1-bemd-$imfIndex.pdf"
        val f = new File(folder, n)
        RInterface.scatterPlot(f, Transformations.sampleSequence(orderedTs zip xIMF))
      }

      for ((yIMF, imfIndex) <- yIMFs.zipWithIndex.drop(yIMFs.size - nbIMFtoPlot)) {
        val n = s"$p2-bemd-$imfIndex.pdf"
        val f = new File(folder, n)
        RInterface.scatterPlot(f, Transformations.sampleSequence(orderedTs zip yIMF))
      }
    }
  }
}
