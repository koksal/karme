package karme.inference

import karme.DiscreteExperiment
import karme.RInterface

case class FunChisqResult(
  source: String, 
  target: String, 
  lag: Int, 
  score: FunChisqScore
)

case class FunChisqScore(
  statistic: Double, 
  pValue: Double, 
  estimate: Double
)

object FunChisq {
  def score(xValues: Seq[Int], yValues: Seq[Int], pairs: Seq[(Int, Int)]): FunChisqScore = {
    val contTable = ContingencyTable.fromPairs(xValues, yValues, pairs)
    // println("Cont table: " + contTable)
    RInterface.funChisq(contTable)
  }

  def writeFunChisqResults(f: java.io.File, resByGroup: Seq[Seq[FunChisqResult]]) = {
    import karme._

    // find out which edges to display
    //    flatten all results
    //    filter by significance
    //    order by statistic
    //    map to (src, tgt) pairs
    //    take distinct edges
    // print across groups for each edge
    //    for each group
    //      if edge has result in group, add it, otherwise mark N/A

    val pValueThreshold = 0.05
    val significantRes = resByGroup.flatten.filter(_.score.pValue <= pValueThreshold)
    val orderedSignificantRes = significantRes.sortBy(_.score.statistic).reverse
    val edgeList = orderedSignificantRes.map(res => (res.source, res.target)).distinct.toList

    if (!edgeList.isEmpty) {
      val tuples = for ((source, target) <- edgeList) yield {
        val rs = for (groupRes <- resByGroup.toList) yield {
          val res = groupRes.find(r => r.source == source && r.target == target) match {
            case Some(FunChisqResult(x, y, lag, score)) => 
              if (score.pValue <= pValueThreshold) score.statistic.toString else ""
            case None => ""
          }
          res
        }
        val tuple = scala.collection.mutable.LinkedHashMap(
          "source" -> source,
          "target" -> target
        )
        for ((r, groupIdx) <- rs.zipWithIndex) {
          tuple += (groupIdx.toString -> r)
        }
        tuple
      }
      FileReporter.outputTuples(f, tuples)
    } else {
      Util.writeToFile(f, "No significant results.")
    }
  }
}
