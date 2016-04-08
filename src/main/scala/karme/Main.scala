package karme

import java.io.File

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    val reporter = new FileReporter(opts.outFolder, opts.outLabel)

    logOptions(opts, reporter)

    val pseudotimeFile = generatePseudotimes(opts, reporter)

    if (opts.evaluate) {
      evaluate(reporter, pseudotimeFile)
    }
  }

  def logOptions(opts: Options, reporter: FileReporter): Unit = {
    import sext._
    reporter.output("options.txt", opts.valueTreeString)
  }

  def generatePseudotimes(opts: Options, reporter: FileReporter): File = {
    val proteins = Parsers.readProteins(opts.proteinNamesPath)
    val exp = processedExperiment(proteins, opts, reporter)

    val pseudotimes = PseudotimePropagation.propagateLabels(
      reporter,
      exp, 
      opts.propagationAlpha, 
      opts.propagationNbNeighbors, 
      opts.propagationNbIter, 
      opts.propagationTimeWeight,
      opts.useJaccardSimilarity
    )

    val pseudotimeFilename = "pseudotimes.csv"
    val pseudotimeFile = reporter.outFile(pseudotimeFilename)

    reporter.outputTuples(pseudotimeFile, exp.toTuplesWithPseudotime(pseudotimes))
    RInterface.plotPseudotimes(reporter, pseudotimeFile, opts.proteinNamesPath)

    pseudotimeFile
  }

  def evaluate(reporter: FileReporter, pseudotimeFile: File): Double = {
    RInterface.spearman(reporter, "Pseudotime", "ActualTime", pseudotimeFile)
  }

  def processedExperiment(proteins: Seq[String], opts: Options, reporter: FileReporter): Experiment = {
    val exp = if (opts.simulate) {
      RInterface.generateSimulatedData(
        reporter, opts.proteinNamesPath, proteins, opts.speedCoefSD, opts.noiseSD, opts.seed
      )
    } else {
      assert(opts.experimentPath != null)
      Parsers.readExperiment(proteins, opts.experimentPath)
    }

    transformedExperiment(exp, opts)
  }

  def transformedExperiment(inputExp: Experiment, opts: Options): Experiment = {
    var res = inputExp

    // transformations that filter measurements
    opts.sampleCount match {
      case Some(count) => res = Transformations.sampleTimePoints(res, opts.seed, count)
      case None =>
    }

    if (opts.filterPositive) {
      res = Transformations.allPositive(res)
    }

    opts.maxTime match {
      case Some(mt) => res = Transformations.filterUntilTime(res, mt)
      case None =>
    }

    res = Transformations.arcsinhValues(res, opts.arcsinhFactor)
    // after arcsinh, check for infinite terms
    res = Transformations.allFinite(res)

    res = Transformations.normalizeValues(res)
    res = Transformations.arcsinhTime(res, opts.arcsinhFactor)
    res = Transformations.normalizeTime(res)

    res
  }

}
