package karme.printing

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import karme.synthesis.Transitions.Transition

object TransitionLogger {

  def saveToFile(transitions: Iterable[Transition], file: File): Unit = {
    val writer = CSVWriter.open(file)

    val header = List("label", "output", "weight", "input")

    val rows = for (
      transition <- transitions.toList.sortBy(_.weight).reverse
    ) yield {
      List(transition.label, transition.outputString, transition.weight,
        transition.input)
    }

    writer.writeAll(header +: rows)
  }

}
