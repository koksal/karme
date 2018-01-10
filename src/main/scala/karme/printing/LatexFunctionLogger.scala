package karme.printing

import java.io.File

import karme.synthesis.SynthesisResult
import karme.synthesis.Transitions.Transition
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
    val header = List("Gene", "Functions", "Weight sum")
    val commonGroupHeaders = Set("Gene", "Weight sum")

    val sortedLabels = labelToResults.keySet.toList.sorted

    val rowGroups = (for (label <- sortedLabels) yield {
      val latexifiedLabel =
        LatexPrinting.latexMath(LatexPrinting.latexifyId(label))
      val labelResults = labelToResults(label)
      for (result <- labelResults.toList) yield {
        val transitionWeightSum = weightSum(result.transitions)

        for (fe <- result.functions.toList) yield {
          Map(
            "Gene" -> latexifiedLabel,
            "Functions" -> FunExprPrettyPrinter.printLaTeX(fe),
            "Weight sum" -> transitionWeightSum
          )
        }
      }
    }).flatten

    LatexTablePrinter.print(header, rowGroups, commonGroupHeaders)
  }

  private def weightSum(ts: Set[Transition]): Double = {
    ts.toList.map(_.weight).sum
  }
}
