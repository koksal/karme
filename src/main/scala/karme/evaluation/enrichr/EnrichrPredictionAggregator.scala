package karme.evaluation.enrichr

import java.io.File

import karme.parsing.NamesParser

object EnrichrPredictionAggregator {

  def main(args: Array[String]): Unit = {
    // read all files and combine
    // first argument: list of TFs
    val names = NamesParser(new File(args(0)))

    // canonicalize all terms and genes
    // match each "Term" with each of the TFs
    // take min p-value?

  }

}
