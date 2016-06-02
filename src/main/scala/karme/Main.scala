package karme

import java.io.File

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    val reporter = new FileReporter(opts.outFolder, opts.outLabel)

    logOptions(opts, reporter)

    val proteins = Parsers.readProteins(opts.proteinNamesPath)
    var experiment = processedExperiment(proteins, opts, reporter)
    
    // Discretization
    val discrExp = discretization.Discretization.discretizeExperiment(experiment)
    writeExp(reporter, discrExp, "discrete-experiment.csv")

    // Reordering
    // experiment = PseudotimePropagation.propagateLabels(
    //   reporter, experiment, opts.labelPropagationOpts
    // )
    // val pseudotimeFile = writeExp(reporter, experiment, "pseudotimes.csv")
    // RInterface.plotPseudotimes(reporter, pseudotimeFile, opts.proteinNamesPath)

    // Inference from average values
    val avgExp = Transformations.averageBySamplingTime(experiment)
    val avgDiscrExp = discretization.Discretization.discretizeExperiment(avgExp)
    writeExp(reporter, avgExp, "average-experiment.csv")
    writeExp(reporter, avgDiscrExp, "discrete-average-experiment.csv")

    // Inference by sampling time

    // Inference by reordered values

    if (opts.evaluate) {
      // evaluate(reporter, pseudotimeFile)
    }
  }

  def logOptions(opts: Options, reporter: FileReporter): Unit = {
    import sext._
    reporter.output("options.txt", opts.valueTreeString)
  }

  def computePseudotimesByPropagation(opts: Options, reporter: FileReporter, exp: Experiment): Experiment = {
    PseudotimePropagation.propagateLabels(
      reporter,
      exp, 
      opts.labelPropagationOpts
    )
  }

  def writeExp(reporter: FileReporter, exp: AbsExperiment[_], fn: String): File = {
    val f = reporter.outFile(fn)
    FileReporter.outputTuples(f, exp.toTuples())
    f
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

    if (opts.filterPositive) {
      res = Transformations.allPositive(res)
    }

    opts.maxTime match {
      case Some(mt) => res = Transformations.filterUntilTime(res, mt)
      case None =>
    }

    // res = Transformations.arcsinhValues(res, opts.arcsinhFactor)
    // after arcsinh, check for infinite terms
    res = Transformations.allFinite(res)

    res = Transformations.normalizeValues(res)
    // res = Transformations.arcsinhTime(res, opts.arcsinhFactor)
    res = Transformations.normalizeTime(res)

    // transformations that filter measurements
    opts.sampleCount match {
      case Some(count) => res = Transformations.sampleValueRange(res, opts.seed, count, 100)
      case None =>
    }

    res
  }

}
