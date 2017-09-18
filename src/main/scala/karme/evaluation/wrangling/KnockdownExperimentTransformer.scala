package karme.evaluation.wrangling

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

    val interactionsWithDups = tuples flatMap { tuple =>
      val targets = tuple.head.split(";").filter(!_.startsWith("LOC"))

      val foldChanges = tuple.drop(nonGeneFields.size).map(_.toDouble)

      assert(foldChanges.size == sources.size)
      sources.zip(foldChanges) flatMap {
        case (src, fc) => {
          targets map (t => (mapSourceName(src), t, fc))
        }
      }
    }

    reduceDuplicateInteractions(interactionsWithDups)
  }

  private def reduceDuplicateInteractions(
    tuples: Seq[(String, String, Double)]
  ): Seq[(String, String, Double)] = {
    val pairToTuple = tuples.groupBy(t => (t._1, t._2))
    pairToTuple.toList.map {
      case (pair, tuples) => {
        val reducedW = reduceWeights(pair._1, pair._2, tuples.map(_._3))
        (pair._1, pair._2, reducedW)
      }
    }
  }

  private def reduceWeights(src: String, tgt: String, ws: Seq[Double]) = {
    val min = ws.min
    val max = ws.max
    if (min.signum != max.signum && min != 0 && max != 0) {
      println(
        s"Min max different for: $src -> $tgt. Weights: ${ws.mkString(",")}")
    }
    ws.maxBy(w => math.abs(w))
  }

  private def mapSourceName(s: String): String = {
    val r = """^(\w+)(?: \(KO.*\))?$""".r
    s match {
      case r(name) => name
    }
  }

  def saveTriples(
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
