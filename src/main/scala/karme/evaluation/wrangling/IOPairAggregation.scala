package karme.evaluation.wrangling

import java.io.File

import karme.parsing.IOPairParser
import karme.printing.IOPairLogger
import karme.util.CollectionUtil

object IOPairAggregation {

  def main(args: Array[String]): Unit = {
    val fs = args map (arg => new File(arg))

    val pairsWithCounts = fs.flatMap(f => IOPairParser(f))

    val combinedPairsWithCounts = CollectionUtil.combineCounts(pairsWithCounts)

    val outF = new File("combined-pairs.csv")
    IOPairLogger.logPairsWithCounts(combinedPairsWithCounts, outF)
  }

}
