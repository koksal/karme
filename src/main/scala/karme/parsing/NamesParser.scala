package karme.parsing

import java.io.File

import scala.io.Source

object NamesParser {

  def apply(fs: Seq[File]): Set[String] = {
    (fs map apply).foldLeft(Set.empty[String])(_ union _)
  }

  def apply(f: File): Set[String] = {
    val names = Source.fromFile(f).getLines().toSeq
    names.toSet
  }

}
