package karme

import scala.io.Source

/**
  * Created by ask on 1/16/17.
  */
object NameAnalysis {

  def main(args: Array[String]): Unit = {
    val names1 = Source.fromFile(args(0)).getLines().toSet
    val names2 = Source.fromFile(args(1)).getLines().toSet

    val canonicalNames2 = names2.map(_.toLowerCase())
    val commonNames = names1 filter { n =>
      canonicalNames2.contains(n.toLowerCase())
    }
    println(s"Intersection: ${commonNames.size}")
    println(commonNames.mkString("\n"))

  }

}
