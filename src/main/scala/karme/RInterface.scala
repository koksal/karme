package karme

object RInterface {
  private def runRProgram(progPath: String, args: Seq[String]): Unit = {
    import scala.sys.process._

    val argString = args.mkString(" ")
    println("Invoking R.")
    s"R -f $progPath --args $argString".!!
  }

  def plotPseudotimes(reporter: FileReporter, pseudotimeFileName: String): Unit = {
    val prog = "./scripts/R/plotPseudotimes.R"
    val inputFile = reporter.outFile(pseudotimeFileName)
    val outputFolder = reporter.outFile("plots")
    val args = List(inputFile.getAbsolutePath(), outputFolder.getAbsolutePath())
  
    runRProgram(prog, args)
  }
}
