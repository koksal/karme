package karme.evaluation

import java.io.File

import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.CSVWriter

class KnockdownExperimentTransformer {

  def main(args: Array[String]): Unit = {
    val foldChangeEffects = parseKnockdownFile(new File(args(0)))

    val absFoldChangeEffects = foldChangeEffects map {
      case (src, tgt, fc) => (src, tgt, math.abs(fc))
    }

    // also save a file with source, target, fold change
    saveTriples(foldChangeEffects, new File("fold-changes.csv"))
    saveTriples(absFoldChangeEffects, new File("abs-fold-changes.csv"))
  }

  private def parseKnockdownFile(f: File): Seq[(String, String, Double)] = {
    val reader = CSVReader.open(f)
    val allRows = reader.all()

    val headers = allRows.head
    val tuples = allRows.tail

    val nonGeneFields = List(
      "Gene",
      "TF",
      "Cyokine-activity",
      "Cell-surface",
      "Perturbed"
    )

    assert(headers.take(nonGeneFields.size) == nonGeneFields)
    val sources = headers.drop(nonGeneFields.size)

    tuples flatMap { tuple =>
      val target = tuple.head
      val isTF = tuple(1).toInt == 1
      val foldChanges = tuple.drop(nonGeneFields.size).map(_.toDouble)

      assert(foldChanges.size == sources.size)
      sources.zip(foldChanges) map {
        case (src, fc) => (src, target, fc)
      }
    }
  }

  private def saveTriples(
    triples: Seq[(String, String, Double)],
    f: File
  ): Unit = {
    val triplesAsLists = triples map {
      case (src, tgt, d) => List(src, tgt, d)
    }

    val writer = CSVWriter.open(f)
    writer.writeAll(triplesAsLists)
    writer.close()
  }
}
