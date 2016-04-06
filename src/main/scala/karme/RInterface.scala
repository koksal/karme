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

  def plotPseudotimes(
    reporter: FileReporter, 
    pseudotimeFileName: String,
    proteinsFile: File
  ): Unit = {
    val prog = "./scripts/R/plotPseudotimes.R"
    val pseudotimeFile = reporter.outFile(pseudotimeFileName)
    val outputFolder = reporter.outFile("plots")
    val args = List(
      pseudotimeFile.getAbsolutePath(), 
      proteinsFile.getAbsolutePath(),
      outputFolder.getAbsolutePath()
    )
  
    runRProgram(prog, args)
  }

  def grangerTest(
    reporter: FileReporter, 
    pseudotimeFileName: String,
    proteinsFile: File
  ): Unit = {
    val prog = "./scripts/R/granger.R"
    val pseudotimeFile = reporter.outFile(pseudotimeFileName)
    val outputFolder = reporter.outFile("granger")
    val args = List(
      pseudotimeFile.getAbsolutePath(), 
      proteinsFile.getAbsolutePath(),
      outputFolder.getAbsolutePath()
    )
  
    runRProgram(prog, args)
  }

  // needs to specify out files
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
    val outputFolder = reporter.outFolder
    val args = List(
      proteinsFile.getAbsolutePath(),
      speedCoefSD.toString,
      noiseSD.toString,
      seedValue.toString,
      outputFolder.getAbsolutePath()
    )
    runRProgram(prog, args)
    val observedExpFile = new File(outputFolder, "observed.csv")
    val observedExp = Parsers.readExperiment(proteins, observedExpFile)
    observedExp
  }

  // R will read pseudotime and actualtime cols, and output rho
  def spearman(
    reporter: FileReporter,
    xName: String,
    yName: String,
    pseudotimeFilename: String
  ): Double = {
    val prog = "./scripts/R/correlation.R"
    val pseudotimeFile = reporter.outFile(pseudotimeFilename)
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
    reporter.outputTuples(pairFilename, rows)

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
