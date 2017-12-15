package karme.printing

object LatexTablePrinter {

  def print(
    header: Seq[String], tuples: Seq[Map[String, String]]
  ): String = {
    val sb = new StringBuilder()

    sb append "\\begin{table}[h]"
    sb append "\n"

    sb append "\\centering"
    sb append "\n"

    sb append "\\footnotesize"
    sb append "\n"

    val colFormat = header.map(h => "l").mkString("{", "|", "}")

    sb append s"\\begin{tabular}$colFormat"
    sb append "\n"

    sb append s"${joinCols(header)} \\\\ \\hline"
    sb append "\n"

    for ((tuple, i) <- tuples.zipWithIndex) {
      sb append (joinCols(header map (h => tuple(h))))
      if (i < tuples.size - 1) {
        sb append " \\\\"
      }
      sb append "\n"
    }

    sb append "\\end{tabular}"
    sb append "\n"

    sb append "\\caption{TODO}"
    sb append "\n"

    sb append "\\label{TODO}"
    sb append "\n"

    sb append "\\end{table}"
    sb append "\n"

    sb.toString()
  }

  private def joinCols(cols: Seq[String]): String = cols.mkString(" & ")

}
