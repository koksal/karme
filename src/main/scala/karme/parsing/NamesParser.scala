package karme.parsing

import java.io.File

import scala.io.Source

object NamesParser {

  def parseNameUnion(fs: Seq[File]): Option[Set[String]] = {
    if (fs.isEmpty) {
      None
    } else {
      val nameSets = fs map parseNames
      val union = nameSets.foldLeft(Set.empty[String])(_ union _)
      Some(union)
    }
  }

  def parseNames(f: File): Set[String] = {
    Source.fromFile(f).getLines().toSet
  }

}
