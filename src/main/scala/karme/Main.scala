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

    FileReporter.outputTuples(pseudotimeFile, exp.toTuples())
    RInterface.plotPseudotimes(reporter, pseudotimeFile, opts.proteinNamesPath)

    // val windowSize = 500
    // val movAvgExp = Transformations.movingAverage(exp, windowSize)

    // val visFn = s"vis-data.csv"
    // val visF = reporter.outFile(visFn)

    // val origTuples = exp.toFlattenedTuples(opts.experimentPath.getName(), "Original")
    // val movAvgTuples = movAvgExp.toFlattenedTuples(opts.experimentPath.getName(), "Moving average")

    // FileReporter.outputTuples(
    //   visF, 
    //   origTuples ++ movAvgTuples
    // )

    bemd(reporter, exp)

    pseudotimeFile
  }

  def plotMEMD(reporter: FileReporter, exp: Experiment, memd: Seq[Seq[Seq[Double]]]): Unit = {
    val orderedMs = exp.measurements.sortBy(_.pseudotime)
    val xs = orderedMs.map(_.pseudotime)
    for ((p, i) <- exp.measuredProteins.zipWithIndex) {
      val emd = memd(i)
      println(s"Components for $p: ${emd.size}")
      for ((vs, j) <- emd.zipWithIndex) {
        val n = s"$p-mode-$j.pdf"
        val f = reporter.outFile(n)
        RInterface.scatterPlot(f, xs, vs)
      }
    }
  }

  def bemd(reporter: FileReporter, exp: Experiment) = {
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
    for ((i, j) <- pairs(n).par) {
      val p1 = exp.measuredProteins(i)
      val p2 = exp.measuredProteins(j)
      println(s"Running BEMD for $p1 and $p2")

      val xs = orderedMs.map(_.values(i))
      val ys = orderedMs.map(_.values(j))
      
      val (xIMFs, yIMFs) = MatlabInterface.bemd(xs, ys)

      val folder = reporter.outFile(s"BEMD-$p1-$p2")
      folder.mkdirs

      for ((xIMF, imfIndex) <- xIMFs.zipWithIndex) {
        val n = s"$p1-bemd-$imfIndex.pdf"
        val f = new File(folder, n)
        RInterface.scatterPlot(f, orderedTs, xIMF)
      }

      for ((yIMF, imfIndex) <- yIMFs.zipWithIndex) {
        val n = s"$p2-bemd-$imfIndex.pdf"
        val f = new File(folder, n)
        RInterface.scatterPlot(f, orderedTs, yIMF)
      }
    }
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
