package karme.printing

import java.io.File

import karme.synthesis.SynthesisResult
import karme.util.FileUtil

object LatexFunctionLogger {

  def apply(
    labelToResults: Map[String, Set[SynthesisResult]],
    f: File
  ): Unit = {
    FileUtil.writeToFile(f, table(labelToResults))
  }

  private def table(
    labelToResults: Map[String, Set[SynthesisResult]]
  ): String = {
    val header = List("Gene", "Functions", "Transitions")
    val commonGroupHeaders = Set("Gene", "Transitions")

    val sortedLabels = labelToResults.keySet.toList.sorted

    val rowGroups = (for (label <- sortedLabels) yield {
      val latexifiedLabel = "$" + FunExprPrettyPrinter.latexifyId(label) + "$"
      val labelResults = labelToResults(label)
      for (result <- labelResults.toList) yield {
        for (fe <- result.functions.toList) yield {
          Map(
            "Gene" -> latexifiedLabel,
            "Functions" -> FunExprPrettyPrinter.printLaTeX(fe),
            "Transitions" -> result.transitions.size
          )
        }
      }
    }).flatten

    LatexTablePrinter.print(header, rowGroups, commonGroupHeaders)
  }

}
