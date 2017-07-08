package karme.analysis

import karme.util.NamingUtil

import scala.io.Source

object NameAnalysis {

  def main(args: Array[String]): Unit = {
    val names1 = Source.fromFile(args(0)).getLines().toSet
    val names2 = Source.fromFile(args(1)).getLines().toSet

    val canonicalNames1 = names1.map(NamingUtil.canonicalize)
    val canonicalNames2 = names2.map(NamingUtil.canonicalize)

    val commonNames = canonicalNames1.intersect(canonicalNames2)
    val onlyIn1 = canonicalNames1 -- canonicalNames2
    val onlyIn2 = canonicalNames2 -- canonicalNames1

    println(s"Intersection: ${commonNames.size}")
    println(commonNames.toList.sorted.mkString("\n"))
  }

}
