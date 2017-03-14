package karme

import karme.util.NamingUtil

import scala.io.Source

/**
  * Created by ask on 1/16/17.
  */
object NameAnalysis {

  def main(args: Array[String]): Unit = {
    val names1 = Source.fromFile(args(0)).getLines().toSet
    val names2 = Source.fromFile(args(1)).getLines().toSet

    val canonicalNames2 = names2.map(NamingUtil.canonicalize)
    val commonNames = names1 filter { n =>
      canonicalNames2.contains(NamingUtil.canonicalize(n))
    }
    println(s"Intersection: ${commonNames.size}")
    println(commonNames.mkString("\n"))

  }

}
