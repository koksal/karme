package karme.printing

object LatexTablePrinter {

  def print(
    header: Seq[String],
    rowGroups: Seq[Seq[Map[String, Any]]],
    commonGroupHeaders: Set[String] = Set()
  ): String = {
    val sb = new StringBuilder()

    sb append "\\begin{table}[ht!]"
    sb append "\n"

    sb append "\\centering"
    sb append "\n"

    sb append "\\footnotesize"
    sb append "\n"

    val colFormat = header.map(h => "l").mkString("{", "|", "}")

    sb append s"\\begin{tabular}$colFormat"
    sb append "\n"

    sb append s"${header.mkString(" & ")} \\\\ \\hline"
    sb append "\n"

    for ((rowGroup, groupIndex) <- rowGroups.zipWithIndex) {
      for ((row, rowIndex) <- rowGroup.zipWithIndex) {
        val rowValues = header map { h =>
          if (!commonGroupHeaders.contains(h) || rowIndex == 0) {
            row(h)
          } else {
            ""
          }
        }
        sb append joinCols(rowValues)
        if (groupIndex < rowGroups.size - 1 || rowIndex < rowGroup.size - 1) {
          sb append " \\\\"
        }
        if (groupIndex < rowGroups.size - 1 && rowIndex == rowGroup.size - 1) {
          sb append " \\hline"
        }
        sb append "\n"
      }

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

  private def joinCols(cols: Seq[Any]): String = {
    cols.map { c =>
      LatexPrinting.latexMath(LatexPrinting.latexifyId(c.toString))
    }.mkString(" & ")
  }

}
