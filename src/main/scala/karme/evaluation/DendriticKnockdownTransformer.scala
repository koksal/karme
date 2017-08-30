package karme.evaluation

import java.io.File

import com.github.tototoshi.csv.CSVReader
import karme.util.FileUtil

object DendriticKnockdownTransformer {

  def main(args: Array[String]): Unit = {
    val interactions = parseFile(new File(args(0)))

    val nonZeroHits = interactions filter {
      case (src, tgt, hit) => hit != 0
    }

    KnockdownExperimentTransformer.saveTriples(nonZeroHits,
      new File("hits.csv"))

    logSourcesAndTargets(nonZeroHits)
  }

  private def logSourcesAndTargets(hits: Seq[(String, String, Double)]) = {
    val sources = hits.map(_._1).distinct.sorted
    val targets = hits.map(_._2).distinct.sorted

    FileUtil.writeToFile(new File("sources.txt"), sources.mkString("\n"))
    FileUtil.writeToFile(new File("targets.txt"), targets.mkString("\n"))
  }

  private def parseFile(f: File): Seq[(String, String, Double)] = {
    val reader = CSVReader.open(f)
    val allRows = reader.all()

    val headers = allRows.head
    val tuples = allRows.tail

    val nonGeneFields = List("Target_genes")

    assert(headers.take(nonGeneFields.size) == nonGeneFields)
    val sources = headers.drop(nonGeneFields.size)

    tuples flatMap { tuple =>
      val target = tuple.head
      val hits = tuple.drop(nonGeneFields.size).map(_.toDouble)

      assert(hits.size == sources.size)

      sources.zip(hits) map {
        case (src, hit) => (src, target, hit)
      }
    }
  }

}
