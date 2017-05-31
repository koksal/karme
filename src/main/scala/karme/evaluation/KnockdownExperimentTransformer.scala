package karme.evaluation

import java.io.File

import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.CSVWriter

object KnockdownExperimentTransformer {

  def main(args: Array[String]): Unit = {
    val foldChangeEffects = parseKnockdownFile(new File(args(0)))
    val nonZeroFoldChangeEffects = foldChangeEffects filter {
      case (src, tgt, fc) => math.abs(fc) > 0
    }

    val absFoldChangeEffects = nonZeroFoldChangeEffects map {
      case (src, tgt, fc) => (src, tgt, math.abs(fc))
    }

    // also save a file with source, target, fold change
    saveTriples(nonZeroFoldChangeEffects, new File("fold-changes.csv"))
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
      val targets = tuple.head.split(";").filter(!_.startsWith("LOC"))

      val foldChanges = tuple.drop(nonGeneFields.size).map(_.toDouble)

      assert(foldChanges.size == sources.size)
      sources.zip(foldChanges) flatMap {
        case (src, fc) => {
          targets map (t => (src, t, fc))
        }
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
