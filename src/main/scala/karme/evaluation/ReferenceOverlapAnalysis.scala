package karme.evaluation

import karme.{ArgHandling, PredictionLibrary, Reporter}

object ReferenceOverlapAnalysis {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    val reporter = new Reporter(opts.reporterOpts)

    val evalCtx = EvaluationContext.fromOptions(opts.evalOpts)

    for {
      i <- 0 until evalCtx.references.size
      j <- i + 1 until evalCtx.references.size
    } {
      findOverlap(evalCtx.references(i), evalCtx.references(j))
    }
  }

  def findOverlap(
    l1: PredictionLibrary, l2: PredictionLibrary
  ): Unit = {
    println(s"${l1.id} and ${l2.id}")

    val ps1 = l1.ioPairs
    val ps2 = l2.ioPairs

    findOverlap(ps1, ps2)
  }

  def findOverlap(
    ps1: Set[(String, String)], ps2: Set[(String, String)]
  ): Unit = {
    val nbCommon = ps1.intersect(ps2).size
    val nbTotal = ps1.union(ps2).size
    val jacSimilarity = nbCommon.toDouble / nbTotal.toDouble

    println(s"|A| = ${ps1.size}")
    println(s"|B| = ${ps2.size}")
    println(s"|A && B|: $nbCommon")
    println(s"|A || B|: $nbTotal")
    println(s"Jaccard: $jacSimilarity")
    println()
  }
}
