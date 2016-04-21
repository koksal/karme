package karme

import java.io.File
import com.github.tototoshi.csv._

object MatlabInterface {
  // matlab -nodisplay -nosplash -nodesktop -r "fid = fopen('matlab-test.txt', 'w'); fprintf(fid, 'test'); fclose(fid); exit; "
  private def runMatlab(function: String, args: Seq[String]): Unit = {
    import scala.sys.process._

    val matlabScriptPath = "./scripts/matlab"
    val commaSepArgs = args.map(arg => "'" + arg + "'").mkString(",")
    // val mScript = "$function($commaSepArgs); exit;"
    val mScript = s"addpath('$matlabScriptPath');" + function + "(" + commaSepArgs + ");exit;"
    val debug = "1 + 2"
    val command = "matlab -nodisplay -nosplash -nodesktop -r " + mScript
    println("Invoking Matlab: " + command)
    command.!!
  }

  private def read3DMatrix(f: File): Seq[Seq[Seq[Double]]] = {
    val reader = CSVReader.open(f)
    val tuples = reader.allWithHeaders()

    val dim1Tuples = tuples.groupBy(t => t("dim1"))

    for ((dim1Key, dim1Values) <- dim1Tuples.toSeq) yield {
      val dim2Tuples = dim1Values.groupBy(t => t("dim2"))
      for ((dim2Key, dim2Values) <- dim2Tuples.toSeq) yield {
        val vs = dim2Values.map(t => t("value").toDouble)
        vs
      }
    }
  }

  def memd(exp: Experiment): Seq[Seq[Seq[Double]]] = {
    val cellValues = exp.measurements.map(_.values)
    memd(cellValues.transpose)
  }

  def memd(xss: Seq[Seq[Double]]): Seq[Seq[Seq[Double]]] = {
    // TODO refactor with RInterface
    val inF = RInterface.tempFile()
    val outF = RInterface.tempFile()

    val rows = xss.transpose
    val writer = com.github.tototoshi.csv.CSVWriter.open(inF)
    writer.writeAll(rows)
    writer.close()

    val args = List(inF.getAbsolutePath(), outF.getAbsolutePath())
    runMatlab("run_memd", args)

    read3DMatrix(outF)
  }
}
