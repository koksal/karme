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

  private def tempFile(): File = {
    val f = File.createTempFile("temp", ".tmp")
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
    val result = Parsers.readSpearman(rOutputFile)
    result
  }

  private def writeVector(xs: Seq[Double], f: File): Unit = {
    Util.writeToFile(f, xs.mkString("\n"))
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

    // TODO write input files
    writeVector(xs, inValueF)
    writeVector(ts, inTimeF)

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
    val rows = neighbors.toSeq.flatMap{ case (i, js) => 
      js.map{ j => 
        val first = ms(i)
        val second = ms(j)
        Map("x" -> first.actualTime.toString, "y" -> second.time.toString)
      }
    }
    val pairFilename = s"$name-neighbors.csv"
    val pairFile = reporter.outFile(pairFilename)
    reporter.outputTuples(pairFile, rows)

    val prog = "./scripts/R/scatterPlot.R"
    val outputFolder = reporter.outFile("plots")
    val outFile = reporter.outFile(pairFilename)
    val args = List(
      outFile.getAbsolutePath(),
      outputFolder.getAbsolutePath(),
      name
    )
    runRProgram(prog, args)
  }
}
