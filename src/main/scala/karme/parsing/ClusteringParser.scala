package karme.parsing

import java.io.File

import com.github.tototoshi.csv.CSVReader
import karme.util.MapUtil

object ClusteringParser {

  def apply(f: File): Map[String, Set[String]] = {
    var clustering = Map[String, Set[String]]()

    val reader = CSVReader.open(f)
    val allRows = reader.all()
    val headers = allRows.head
    val tuples = allRows.tail

    assert(headers == List("id", "cluster"))

    for (List(id, cluster) <- tuples) {
      clustering = MapUtil.addBinding(clustering, cluster, id)
    }

    clustering
  }

}
