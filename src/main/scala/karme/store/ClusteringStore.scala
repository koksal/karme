package karme.store

import java.io.File

import io.circe.parser._
import io.circe.syntax._
import karme.util.FileUtil

class ClusteringStore(parent: File) {

  val fileName = "gene-clustering.json"
  val file = new File(parent, fileName)

  def store(clustering: Map[String, Set[String]]): Unit = {
    println(clustering.asJson.spaces2)
  }

  def read: Map[String, Set[String]] = {
    decode[Map[String, Set[String]]](FileUtil.readContent(file)) match {
      case Left(err) => sys.error(err.getMessage)
      case Right(clustering) => clustering
    }
  }

}
