package karme.printing

import java.io.File

import karme.synthesis.SynthesisResult
import karme.util.FileUtil

object SynthesisResultLogger {

  def apply(
    labelToResults: Map[String, Set[SynthesisResult]], f: File
  ): Unit = {
    val contentPerLabel = labelToResults map {
      case (label, results) => {
        val resStr = results.map(resultStr).mkString("\n\n")
        s"""Label: $label
           |$resStr
         """.stripMargin
      }
    }
    FileUtil.writeToFile(f, contentPerLabel.mkString("\n\n"))
  }

  def resultStr(r: SynthesisResult): String = {
    s"Transitions used: ${r.transitions.size}\n" +
      r.functions.map(FunExprPrettyPrinter.printPlain).mkString("\n")
  }

}
