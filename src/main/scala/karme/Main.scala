package karme

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    val reporter = new FileReporter(opts.outFolder, opts.outLabel)
    val proteins = Parsers.readProteins(opts.proteinNamesPath)
    var exp = Parsers.readExperiment(proteins, opts.experimentPath)

    exp = Transformations.arcsinh(exp, opts.arcsinhFactor)
    if (opts.filterPositive) {
      exp = Transformations.allPositive(exp)
    }
    opts.maxTime match {
      case Some(mt) => exp = Transformations.filterUntilTime(exp, mt)
      case None =>
    }
    if (opts.sample) {
      exp = Transformations.sampleTimePoints(exp, opts.seed)
    }

    exp = Transformations.normalize(exp)

    // TODO add CLI argument with seed
    // exp = Transformations.shuffleTimeLabels(exp, seed)

    val pseudotimes = PseudotimePropagation.propagateLabels(
      exp, 
      opts.propagationAlpha, 
      opts.propagationNbNeighbors, 
      opts.propagationNbIter, 
      opts.propagationSplitTime
    )

    val pseudotimeFilename = "pseudotimes.csv"
    reporter.outputTuples(pseudotimeFilename, exp.toTuplesWithPseudotime(pseudotimes))

    RInterface.plotPseudotimes(reporter, pseudotimeFilename)
  }
}
