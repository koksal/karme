package karme

object Evaluation {
  // harness loop for evaluating different parameters within Scala.
  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)

    evaluateRange(opts)
  }

  def evaluateRange(mainOpts: Options): Unit = {
    // algorithm parameters to vary
    val iter        = 10
    val timeWeight  = 0
    val nbNeigh     = 10
    val seed        = 1

    val r: List[Double] = List(0.25, 2, 5)
    val speedSDRange = r 
    val noiseSDRange = r
    val rhoValues: Map[(Double, Double), Double] = (for {
      speedSD <- speedSDRange
      noiseSD <- noiseSDRange
    } yield {
      val lbl = label(iter, timeWeight, nbNeigh, speedSD, noiseSD)

      val runOpts = mainOpts.copy(
        propagationNbIter = iter,
        propagationTimeWeight = timeWeight,
        propagationNbNeighbors = nbNeigh,
        seed = Some(seed),
        speedCoefSD = speedSD,
        noiseSD = noiseSD,
        outLabel = Some(lbl)
      )

      val runReporter = new FileReporter(runOpts.outFolder, runOpts.outLabel)

      val ptFile = Main.generatePseudotimes(runOpts, runReporter)
      val rho = Main.evaluate(runReporter, ptFile)

      (speedSD, noiseSD) -> rho
    }).toMap

    // print table with explored values
    val reporter = new FileReporter(mainOpts.outFolder, mainOpts.outLabel)
    val tableFile = reporter.outFile("rhoTable.csv")
    val tableValues = speedSDRange.map{ ssd =>
      noiseSDRange.map{ nsd =>
        rhoValues((ssd, nsd))
      }
    }
    reporter.writeTable(tableFile, "speed", "noise", speedSDRange, noiseSDRange, tableValues)
  }

  def label(iter: Int, timeWeight: Double, nbNeigh: Int, speedSD: Double, noiseSD: Double) = {
    s"iter-$iter-tw-$timeWeight-nb-$nbNeigh-speed-$speedSD-noise-$noiseSD"
  }
}
