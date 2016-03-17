package karme

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    val reporter = new FileReporter(opts.outFolder, opts.outLabel)
    val proteins = Parsers.readProteins(opts.proteinNamesPath)
    var exp = if (opts.simulate) {
      RInterface.generateSimulatedData(
        reporter, opts.proteinNamesPath, proteins, opts.speedCoefSD, opts.noiseSD, opts.seed
      )
    } else {
      assert(opts.experimentPath != null)
      Parsers.readExperiment(proteins, opts.experimentPath)
    }

    exp = Transformations.arcsinh(exp, opts.arcsinhFactor)
    if (opts.filterPositive) {
      exp = Transformations.allPositive(exp)
    }
    opts.maxTime match {
      case Some(mt) => exp = Transformations.filterUntilTime(exp, mt)
      case None =>
    }
    opts.sampleCount match {
      case Some(count) => exp = Transformations.sampleTimePoints(exp, opts.seed, count)
      case None =>
    }

    exp = Transformations.normalizeProteins(exp)

    // TODO add CLI argument with seed
    // exp = Transformations.shuffleTimeLabels(exp, seed)

    val pseudotimes = PseudotimePropagation.propagateLabels(
      exp, 
      opts.propagationAlpha, 
      opts.propagationNbNeighbors, 
      opts.propagationNbIter, 
      opts.propagationTimeWeight,
      opts.propagationSplitTime
    )

    import sext._
    reporter.output("options.txt", opts.valueTreeString)

    val pseudotimeFilename = "pseudotimes.csv"
    reporter.outputTuples(pseudotimeFilename, exp.toTuplesWithPseudotime(pseudotimes))

    RInterface.plotPseudotimes(reporter, pseudotimeFilename, opts.proteinNamesPath)

    if (opts.evaluate) {
      val score = Evaluation.evaluateReordering(
        exp, pseudotimes
      )

      val valuesToPrint = List(
        opts.propagationNbIter.toString, 
        opts.speedCoefSD.toString,
        opts.noiseSD.toString,
        opts.propagationTimeWeight.toString,
        opts.propagationNbNeighbors.toString,
        score.toString
      )
      println(s"SCORE: ${valuesToPrint.mkString("\t")}")
    }
  }
}
