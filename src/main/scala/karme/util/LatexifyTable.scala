package karme.util

import java.io.File

import karme.printing.LatexTablePrinter

object LatexifyTable {

  def main(args: Array[String]): Unit = {
    for (arg <- args) {
      latexify(arg)
    }
  }

  def latexify(fn: String): Unit = {
    val f = new File(fn)
    val (header, table) = TSVUtil.readHeadersAndData(f)

    FileUtil.writeToFile(
      new File(fn + ".tex"),
      LatexTablePrinter.print(header, Seq(table))
    )
  }

}
