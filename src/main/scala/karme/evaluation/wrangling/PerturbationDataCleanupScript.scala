package karme.evaluation.wrangling

import java.io.File

import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.CSVWriter

object PerturbationDataCleanupScript {

  val TF_FIELD = "TF"
  val GENE_FIELD = "Gene"

  def main(args: Array[String]): Unit = {
    val f = new File(args(0))

    val reader = CSVReader.open(f)
    val (headers, data) = reader.allWithOrderedHeaders()
    reader.close()

    assert(headers == List(TF_FIELD, GENE_FIELD))

    var mappedPairs = Set[List[String]]()

    for (row <- data) {
      val tf = row(TF_FIELD)
      val genes = row(GENE_FIELD).split(";")
      val relevantTargets = genes filter (g => !g.startsWith("LOC"))

      for (target <- relevantTargets) {
        mappedPairs += List(tf, target)
      }
    }

    val outF = new File("mapped-network.csv")
    val writer = CSVWriter.open(outF)

    writer.writeAll(headers +: (mappedPairs.toList))
    writer.close()
  }
}
