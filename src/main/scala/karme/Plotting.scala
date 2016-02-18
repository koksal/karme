package karme

object Plotting {
  def plotPercentages(name: String, data: Map[String, List[Double]]): Unit = {
    val dataList = data.toList
    val script = gnuplotPercentageScript(name, dataList.map(_._1))
    writeScript(name, script)
    writeData(name, dataList)
    invokeGnuplot(name)
    cleanup(name)
  }

  def writeScript(name: String, script: String): Unit = {
    val fn = scriptFileName(name)
    val f = new java.io.File(fn)
    Util.writeToFile(f, script)
  }

  def scriptFileName(name: String): String = {
    "script.tmp"
  }

  def outputFileName(name: String): String = {
    s"$name.svg"
  }

  def dataFileName(name: String): String = {
    "data.tmp"
  }

  def writeData(name: String, data: List[(String, List[Double])]): Unit = {
    val dataMatrix: List[List[Double]] = data.map(_._2)
    val xposed = dataMatrix.transpose
    val toWrite = xposed.map{ row => row.mkString(",") }.mkString("\n")
    val fn = dataFileName(name)
    val f = new java.io.File(fn)
    Util.writeToFile(f, toWrite)
  }
  
  def gnuplotPercentageScript(name: String, graphLabels: List[String]): String = {
    val sb = new StringBuffer()

    sb append s"set title '${cleanText(name)}'\n"
    sb append "set xlabel 'step'\n"
    sb append "set ylabel 'activity percentage'\n"
    sb append "set terminal svg size 800,400\n"
    sb append "set key outside\n"
    sb append s"set output '${outputFileName(name)}'\n"
    sb append "set datafile separator ','\n"
    val plotStrs = graphLabels.zipWithIndex map { case (label, idx) =>
      s"'${dataFileName(name)}' using ${idx + 1} with lines title '${cleanText(label)}'"
    }
    val plotConcat = plotStrs.mkString(", ")
    sb append s"plot $plotConcat\n"

    sb.toString
  }

  def cleanText(t: String): String = t.replaceAll("_", " ")

  def invokeGnuplot(name: String): Unit = {
    import scala.sys.process._

    val scriptfn = scriptFileName(name)
    s"gnuplot $scriptfn".!!
  }

  def cleanup(name: String): Unit = {
    import scala.sys.process._

    val files = List(scriptFileName(name), dataFileName(name))
    for (f <- files) {
      s"rm $f".!!
    }
    
  }
}
