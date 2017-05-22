package karme.store

import java.io.File

import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import karme.transformations.EdgePrecedence
import karme.util.FileUtil

class EdgePrecedenceStore(parent: File) {

  val fileName = "edge-precedence.json"
  val file = new File(parent, fileName)

  def store(eps: Seq[EdgePrecedence]): Unit = {

    FileUtil.writeToFile(file, eps.asJson.spaces2)
  }

  def read: Seq[EdgePrecedence] = {
    decode[Seq[EdgePrecedence]](FileUtil.readContent(file)) match {
      case Left(err) => sys.error(err.getMessage)
      case Right(eps) => eps
    }
  }

}
