package karme

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    val reporter = new FileReporter(opts.outFolder, opts.outLabel)

    import sext._
    reporter.output("options.txt", opts.valueTreeString)

    val proteins = Parsers.readProteins(opts.proteinNamesPath)
    var exp = if (opts.simulate) {
      RInterface.generateSimulatedData(
        reporter, opts.proteinNamesPath, proteins, opts.speedCoefSD, opts.noiseSD, opts.seed
      )
    } else {
      assert(opts.experimentPath != null)
      Parsers.readExperiment(proteins, opts.experimentPath)
    }

    // transformations that filter measurements
    opts.sampleCount match {
      case Some(count) => exp = Transformations.sampleTimePoints(exp, opts.seed, count)
      case None =>
    }

    if (opts.filterPositive) {
      exp = Transformations.allPositive(exp)
    }

    opts.maxTime match {
      case Some(mt) => exp = Transformations.filterUntilTime(exp, mt)
      case None =>
    }

    // transform values for all further processing
    exp = Transformations.arcsinhValues(exp, opts.arcsinhFactor)
    exp = Transformations.normalizeValues(exp)
    exp = Transformations.arcsinhTime(exp, opts.arcsinhFactor)
    exp = Transformations.normalizeTime(exp)

    val pseudotimes = PseudotimePropagation.propagateLabels(
      reporter,
      exp, 
      opts.propagationAlpha, 
      opts.propagationNbNeighbors, 
      opts.propagationNbIter, 
      opts.propagationTimeWeight,
      opts.useJaccardSimilarity,
      opts.propagationSplitTime
    )

    val scaledPseudotimes = RangeScaling.scalePseudotimes(
      pseudotimes,
      exp.measurements.map(_.time).toArray
    )

    val pseudotimeFilename = "pseudotimes.csv"
    reporter.outputTuples(pseudotimeFilename, exp.toTuplesWithPseudotime(scaledPseudotimes))

    RInterface.plotPseudotimes(reporter, pseudotimeFilename, opts.proteinNamesPath)

    if (opts.evaluate) {
      val spearmanRho = RInterface.spearman(reporter, "Pseudotime", "ActualTime", pseudotimeFilename)

      val valuesToPrint = List(
        opts.propagationNbIter.toString, 
        opts.speedCoefSD.toString,
        opts.noiseSD.toString,
        opts.propagationTimeWeight.toString,
        opts.propagationNbNeighbors.toString,
        spearmanRho.toString
      )
      println(s"EVALUATION: ${valuesToPrint.mkString("\t")}")
    }
  }
}
