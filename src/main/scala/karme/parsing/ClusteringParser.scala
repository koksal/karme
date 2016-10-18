package karme.parsing

import java.io.File

import com.github.tototoshi.csv.CSVReader

import scala.collection.mutable

object ClusteringParser {
  def parse(f: File): mutable.MultiMap[String, String] = {
    val clustering = new mutable.HashMap[String, Set[String]]()
      with mutable.MultiMap[String, String]

    val reader = CSVReader.open(f)
    val allRows = reader.all()
    val headers = allRows.head
    val tuples = allRows.tail

    assert(headers == List("id", "cluster"))

    for (List(id, cluster) <- tuples) {
      clustering.addBinding(cluster, id)
    }

    clustering
  }
}
