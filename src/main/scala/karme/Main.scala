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
    var exp = processedExperiment(proteins, opts, reporter)

    exp = PseudotimePropagation.propagateLabels(
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

    reporter.outputTuples(pseudotimeFile, exp.toTuples())
    // RInterface.plotPseudotimes(reporter, pseudotimeFile, opts.proteinNamesPath)

    // val windowSize = 500
    // val movAvgExp = Transformations.movingAverage(exp, windowSize)
    // val maPseudotimeFn = s"vis-data.csv"
    // val maPseudotimeFile = reporter.outFile(maPseudotimeFn)

    // reporter.outputTuples(maPseudotimeFile, movAvgExp.toFlattenedTuples())

    for ((p, i) <- exp.measuredProteins.zipWithIndex) {
      val xs = exp.measurements.map(_.values(i))
      val ts = exp.measurements.map(_.pseudotime)
      val res = RInterface.emd(xs, ts)
      println("EMD: ")
      println(s"IMFs: ${res._1.size}")
    }

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
