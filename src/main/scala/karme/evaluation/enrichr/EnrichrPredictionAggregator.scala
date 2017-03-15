package karme.evaluation.enrichr

import java.io.File

import karme.parsing.NamesParser
import karme.util.NamingUtil

object EnrichrPredictionAggregator {

  def main(args: Array[String]): Unit = {
    // first argument: list of TFs
    val names = NamesParser(new File(args(0)))
    val canonicalNames = names map NamingUtil.canonicalize

    // remaining arguments: Enrichr output files
    val enrichrFiles = args.tail map (new File(_))
    println(s"Parsing ${enrichrFiles.size} files.")
    val enrichrParser = new EnrichrOutputParser(canonicalNames)
    val predictions = enrichrFiles flatMap enrichrParser.parse

    val outputFile = new File("enrichr-predictions.csv")
    EnrichrPredictionsLogger.saveToFile(predictions, outputFile)
  }

}
