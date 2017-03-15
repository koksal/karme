package karme.evaluation.enrichr

import java.io.File

import karme.parsing.NamesParser
import karme.util.NamingUtil

object EnrichrPredictionAggregator {

  def main(args: Array[String]): Unit = {
    // read all files and combine
    // first argument: list of TFs
    val names = NamesParser(new File(args(0)))
    val canonicalNames = names map NamingUtil.canonicalize

    // go through each file and extract TSV
    val enrichrParser = new EnrichrOutputParser(canonicalNames)
    val enrichrFiles = args.tail map (new File(_))
    val predictions = enrichrFiles flatMap enrichrParser.parse

    // canonicalize all terms and genes
    // match each "Term" with each of the TFs
    // take min p-value?

  }

}
