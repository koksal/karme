package karme.printing

import java.io.File

import karme.synthesis.FunctionTrees
import karme.synthesis.SynthesisResult
import karme.util.FileUtil

object SynthesisResultLogger {

  def apply(labelToResult: Map[String, SynthesisResult], f: File): Unit = {
    val contentPerLabel = labelToResult map {
      case (label, result) => {
        s"""Label: $label
           |${resultStr(result)}
         """.stripMargin
      }
    }
    FileUtil.writeToFile(f, contentPerLabel.mkString("\n\n"))
  }

  private def resultStr(r: SynthesisResult): String = {
    r.functions.map(FunExprPrettyPrinter.apply).mkString("\n")
  }

}
