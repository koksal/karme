package karme

object RInterface {
  private def runRProgram(progPath: String, args: Seq[String]): Unit = {
    import scala.sys.process._

    val argString = args.mkString(" ")
    println("Invoking R.")
    s"R -f $progPath --args $argString".!!
  }

  def plotPseudotimes(
    reporter: FileReporter, 
    pseudotimeFileName: String,
    proteinsFile: java.io.File
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
    proteinsFile: java.io.File
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
}
