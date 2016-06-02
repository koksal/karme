package karme

import java.io.File

object RInterface {
  private def runRProgram(progPath: String, args: Seq[String]): Unit = {
    import scala.sys.process._

    val argString = args.mkString(" ")
    val cmd = s"Rscript $progPath $argString"
    println("Invoking R: " + cmd)
    cmd.!!
  }

  def tempFile(): File = {
    val f = File.createTempFile("karme", ".tmp")
    f.deleteOnExit()
    f
  }

  def plotPseudotimes(
    reporter: FileReporter, 
    pseudotimeFile: File,
    proteinsFile: File
  ): Unit = {
    val prog = "./scripts/R/plotPseudotimes.R"
    val outputFolder = reporter.outFile("plots")
    val args = List(
      pseudotimeFile.getAbsolutePath(), 
      proteinsFile.getAbsolutePath(),
      outputFolder.getAbsolutePath()
    )
  
    runRProgram(prog, args)
  }

  // TODO pass a a temp file to R to get back results
  def grangerTest(
    reporter: FileReporter, 
    pseudotimeFile: File,
    proteinsFile: File
  ): Unit = {
    val prog = "./scripts/R/granger.R"
    val outputFolder = reporter.outFile("granger")
    val args = List(
      pseudotimeFile.getAbsolutePath(), 
      proteinsFile.getAbsolutePath(),
      outputFolder.getAbsolutePath()
    )
  
    runRProgram(prog, args)
  }

  // TODO use temp files for R output
  def generateSimulatedData(
    reporter: FileReporter,
    proteinsFile: File,
    proteins: Seq[String],
    speedCoefSD: Double,
    noiseSD: Double,
    seed: Option[Int]
  ): Experiment = {
    val seedValue = seed match {
      case Some(sv) => sv
      case None => println("Using default seed value."); 0
    }
    val prog = "./scripts/R/simulation.R"
    val outputFile = reporter.outFile("observed.csv")
    val args = List(
      proteinsFile.getAbsolutePath(),
      speedCoefSD.toString,
      noiseSD.toString,
      seedValue.toString,
      outputFile.getAbsolutePath()
    )
    runRProgram(prog, args)
    val observedExp = Parsers.readExperiment(proteins, outputFile)
    observedExp
  }

  // R will read pseudotime and actualtime cols, and output rho
  def spearman(
    reporter: FileReporter,
    xName: String,
    yName: String,
    pseudotimeFile: File
  ): Double = {
    val prog = "./scripts/R/correlation.R"
    val rOutputFile = reporter.outFile("spearman.txt")
    val args = List(
      pseudotimeFile.getAbsolutePath(),
      xName,
      yName,
      rOutputFile.getAbsolutePath()
    )
    runRProgram(prog, args)
    Parsers.readDouble(rOutputFile)
  }

  def emd(
    xs: Seq[Double],
    ts: Seq[Double]
  ): (Seq[Seq[Double]], Seq[Double]) = {
    val prog = "./scripts/R/emd.R"
    val inValueF = tempFile()
    val inTimeF = tempFile()
    val imfF = tempFile()
    val residueF = tempFile()

    FileReporter.writeVector(xs, inValueF)
    FileReporter.writeVector(ts, inTimeF)

    val args = List(
      inValueF.getAbsolutePath(),
      inTimeF.getAbsolutePath(),
      imfF.getAbsolutePath(),
      residueF.getAbsolutePath()
    )
    runRProgram(prog, args)
    val result = Parsers.readEMD(imfF, residueF)
    result
  }

  def plotNeighborGraph(
    reporter: FileReporter,
    ms: IndexedSeq[CellMeasurement],
    neighbors: Map[Int, Seq[Int]],
    name: String
  ): Unit = {
    val pairs = neighbors.toSeq.flatMap{ case (i, js) => 
      js.map{ j => 
        val first = ms(i)
        val second = ms(j)
        (first.actualTime.toString, second.time.toString)
      }
    }
    val outF = reporter.outFile(s"$name-neighbors-scatter-plot.pdf")
    val (xs, ys) = pairs.unzip
    scatterPlot(outF, xs zip ys)
  }

  def plotEMD(
    reporter: FileReporter,
    exp: Experiment
  ): Unit = {
    val orderedMeasurements = exp.measurements.sortBy(_.pseudotime)
    val ts = orderedMeasurements.map(_.pseudotime)

    for ((p, i) <- exp.measuredProteins.zipWithIndex.par) {
      val xs = orderedMeasurements.map(_.values(i))
      val (imfs, residue) = RInterface.emd(xs, ts)

      // plot each imf with its index, and residue, against ts
      for ((imf, imfIndex) <- imfs.zipWithIndex) {
        val n = s"$p-imf-$imfIndex.pdf"
        val f = reporter.outFile(n)
        scatterPlot(f, Transformations.sampleSequence(ts zip imf))
      }

      val resN = s"$p-residue.pdf"
      val resF = reporter.outFile(resN)
      scatterPlot(resF, Transformations.sampleSequence(ts zip residue))
    }
  }

  def scatterPlot[A, B](
    f: File,
    points: Seq[(A, B)]
  ): Unit = {
    val rows = points.map {
      case (x, y) => Map("x" -> x.toString, "y" -> y.toString)
    }
    scatterPlotRows(f, rows)
  }

  def groupedScatterPlot[A, B, C](
    f: File,
    points: Seq[(A, B, C)]
  ): Unit = {
    val rows = points map {
      case (x, y, g) => Map("x" -> x.toString, "y" -> y.toString, "group" -> g.toString)
    }
    scatterPlotRows(f, rows)
  }

  private def scatterPlotRows(f: File, rows: Seq[Map[String, String]]): Unit = {
    val dataF = tempFile()
    FileReporter.outputTuples(dataF, rows)

    val prog = "./scripts/R/scatterPlot.R"
    val args = List(
      dataF.getAbsolutePath(),
      f.getAbsolutePath()
    )
    runRProgram(prog, args)
  }

  import karme.discretization.DiscretizationResult
  def ckmeans(xs: Seq[Double]): DiscretizationResult = {
    val prog = "./scripts/R/ckmeans.R"
    val inVectorF = tempFile()
    val outVectorF = tempFile()

    FileReporter.writeVector(xs, inVectorF)
    val args = List(
      inVectorF.getAbsolutePath(),
      outVectorF.getAbsolutePath()
    )

    runRProgram(prog, args)
    val discreteValues = karme.Parsers.readIntVector(outVectorF)
    val nbLevels = discreteValues.max - discreteValues.min + 1
    DiscretizationResult(nbLevels, discreteValues)
  }

  import karme.inference.ContingencyTable
  def funChisq(ct: ContingencyTable): Double = {
    val prog = "./scripts/R/funChisq.R"
    val inF = tempFile()
    val outF = tempFile()

    FileReporter.writeMatrix(inF, ct.table)
    val args = List(
      inF.getAbsolutePath(),
      outF.getAbsolutePath()
    )

    runRProgram(prog, args)
    Parsers.readDouble(outF)
  }
} 
