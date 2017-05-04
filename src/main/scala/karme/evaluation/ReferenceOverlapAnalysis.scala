package karme.evaluation

import karme.ArgHandling
import karme.Reporter
import karme.evaluation.enrichr.EnrichrPredictionLibrary

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
    l1: EnrichrPredictionLibrary, l2: EnrichrPredictionLibrary
  ): Unit = {
    println(s"${l1.id} and ${l2.id}")

    val ps1 = BigramEvaluation.libraryPairsByDescendingScore(l1)
    val ps2 = BigramEvaluation.libraryPairsByDescendingScore(l2)

    findOverlap(ps1.toSet, ps2.toSet)
  }

  def findOverlap(
    ps1: Set[(String, String)], ps2: Set[(String, String)]
  ): Unit = {
    val nbCommon = ps1.intersect(ps2).size
    val nbTotal = ps1.union(ps2).size
    val jaccardDistance = nbCommon.toDouble / nbTotal.toDouble

    println(s"|A| = ${ps1.size}")
    println(s"|B| = ${ps2.size}")
    println(s"|A && B|: $nbCommon")
    println(s"|A || B|: $nbTotal")
    println(s"Jaccard: $jaccardDistance")
    println()
  }
}
