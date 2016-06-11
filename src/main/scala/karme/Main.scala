package karme

import java.io.File

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    val reporter = new FileReporter(opts.outFolder, opts.outLabel)

    logOptions(opts, reporter)

    val proteins = Parsers.readProteins(opts.proteinNamesPath)
    var experiment = processedExperiment(proteins, opts, reporter)
    
    // Reordering
    experiment = PseudotimePropagation.propagateLabels(
      reporter, experiment, opts.labelPropagationOpts
    )
    val pseudotimeFile = Util.writeExp(reporter, experiment, "pseudotimes.csv")
    RInterface.plotPseudotimes(reporter, pseudotimeFile, opts.proteinNamesPath)

    // Discretization
    val discrExp = discretization.Discretization.discretizeExperiment(experiment)
    Util.writeExp(reporter, discrExp, "discrete-experiment.csv")

    // Inference
    println("Computing inference from averages...")
    inference.InferenceFromAverages.infer(experiment, reporter)
    println("Computing inference by sampling time...")
    inference.InferenceBySamplingTime.infer(discrExp, reporter)
    println("Computing inference by reordering...")
    inference.InferenceByReordering.infer(discrExp, reporter)

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

    opts.arcsinhFactor match {
      case Some(factor) => {
        res = Transformations.arcsinhValues(res, factor)
        // after arcsinh, check for infinite terms
        res = Transformations.allFinite(res)
        res = Transformations.arcsinhTime(res, factor)
      }
      case None =>
    }

    res = Transformations.normalizeValues(res)
    res = Transformations.normalizeTime(res)

    opts.sampleCount match {
      case Some(count) => res = Transformations.sampleValueRange(res, opts.seed, count, 100)
      case None =>
    }

    res
  }

}
