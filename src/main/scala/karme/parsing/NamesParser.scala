package karme.parsing

import java.io.File

import scala.io.Source

class NamesParser(fs: Seq[File]) {

  val names: Option[Set[String]] = {
    if (fs.isEmpty) {
      None
    } else {
      val nameUnion = (fs map apply).foldLeft(Set.empty[String])(_ union _)
      Some(nameUnion)
    }
  }

  private def apply(f: File): Set[String] = {
    val names = Source.fromFile(f).getLines().toSeq
    names.toSet
  }

}
