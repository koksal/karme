package karme

import java.io.File

object RInterface {
  private def runRProgram(progPath: String, args: Seq[String]): Unit = {
    import scala.sys.process._

    val argString = args.mkString(" ")
    println("Invoking R.")
    s"Rscript $progPath $argString".!!
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
    speedCoefSD: Int,
    noiseSD: Int,
    seed: Option[Int]
  ): Experiment = {
    val seedValue = seed match {
      case Some(sv) => sv
      case None => println("Using default seed value."); 0
    }
    val prog = "./scripts/R/simulation.R"
    val outputFolder = reporter.outFile("simulation")
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

  // needs to specify out files
  def evaluateReordering(
    reporter: FileReporter,
    proteinsFile: File,
    pseudotimeFilename: String,
    seed: Option[Int]
  ) = {
    // read a file with score?
  }
}
