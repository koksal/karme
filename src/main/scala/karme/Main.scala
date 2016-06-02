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
    val pseudotimeFile = writeExp(reporter, experiment, "pseudotimes.csv")
    RInterface.plotPseudotimes(reporter, pseudotimeFile, opts.proteinNamesPath)

    // Discretization
    val discrExp = discretization.Discretization.discretizeExperiment(experiment)
    writeExp(reporter, discrExp, "discrete-experiment.csv")

    // Inference
    runInferenceOnAverages(experiment, reporter)
    runInferenceBySamplingTime(discrExp, reporter)
    runInferenceByReordering(discrExp, reporter)

    if (opts.evaluate) {
      // evaluate(reporter, pseudotimeFile)
    }
  }

  def runInferenceOnAverages(exp: Experiment, reporter: FileReporter) = {
    val avgExp = Transformations.averageBySamplingTime(exp)
    val avgDiscrExp = discretization.Discretization.discretizeExperiment(avgExp)
    writeExp(reporter, avgExp, "average-experiment.csv")
    writeExp(reporter, avgDiscrExp, "discrete-average-experiment.csv")

    val avgScore = inference.FunChisq.scores(avgDiscrExp)
    writeFunChisqResults(reporter.outFile("average-scores.csv"), avgScore)
  }

  def runInferenceBySamplingTime(exp: DiscreteExperiment, reporter: FileReporter) = {
    for (t <- exp.samplingTimes) {
      val msAtT = exp.measurements.filter(_.time == t)
      val expAtT = exp.copy(measurements = msAtT)
      val score = inference.FunChisq.scores(expAtT)
      writeFunChisqResults(reporter.outFile(s"sampling-time-$t-scores.csv"), score)
    }
  }

  def runInferenceByReordering(exp: DiscreteExperiment, reporter: FileReporter) = {
    val nbWindows = 50
    val orderedMs = exp.measurements.sortBy(_.pseudotime)
    val windowSize = orderedMs.size / nbWindows
    val windows = orderedMs.grouped(windowSize)

    for ((window, i) <- windows.zipWithIndex) {
      val windowExp = exp.copy(measurements = window)
      val score = inference.FunChisq.scores(windowExp)
      writeFunChisqResults(reporter.outFile(s"window-$i-scores.csv"), score)
    }
  }

  import inference.FunChisqResult
  def writeFunChisqResults(f: File, res: Map[(String, String), FunChisqResult]) = {
    val pValueThreshold = 0.05
    val significantRes = res.filter(_._2.pValue <= pValueThreshold)

    if (!significantRes.isEmpty) {
      val ordered = significantRes.toSeq.sortBy(_._2.statistic).reverse
      val tuples = ordered.map{
        case ((x, y), fcr) =>
          scala.collection.mutable.LinkedHashMap(
            "x" -> x,
            "y" -> y,
            "statistic" -> fcr.statistic,
            "p-value" -> fcr.pValue,
            "estimate" -> fcr.estimate
          )
      }
      FileReporter.outputTuples(f, tuples)
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
