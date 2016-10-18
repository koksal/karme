package karme.parsing

import java.io.File

import com.github.tototoshi.csv.CSVReader

import scala.collection.mutable.HashMap
import scala.collection.mutable.MultiMap

object ClusteringParser {
  def parse(f: File): MultiMap[String, String] = {
    val clustering =
      new HashMap[String, Set[String]]() with MultiMap[String, String]

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
