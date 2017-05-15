package karme.evaluation

import java.io.File

import karme.ArgHandling
import karme.Reporter
import karme.evaluation.enrichr.{EnrichrPredictionLibrary}
import karme.parsing.IOPairParser
import karme.util.MathUtil
import karme.visualization.HistogramPlotInterface

import scala.util.Random

object IOPairEvaluation {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    val reporter = new Reporter(opts.reporterOpts)

    val evalCtx = EvaluationContext.fromOptions(opts.evalOpts)

    val predsWithCounts = IOPairParser(opts.evalOpts.predictionPairsFile.get)

    for (library <- evalCtx.references) {
      evaluate(predsWithCounts, library, reporter)
    }
  }

  def evaluate(
    predictionsWithCounts: Seq[((String, String), Int)],
    library: EnrichrPredictionLibrary,
    reporter: Reporter
  ): Unit = {
    val hypergeomEval = new HypergeometricEvaluation(reporter)
    hypergeomEval.evaluate(predictionsWithCounts, library)

    // TODO PR eval. Separate predictions into match/nonmatch, feed with counts
  }


  private def plotScoreDist(scores: Seq[Double], f: File): Unit = {
    val labels = scores.map(_ => "none")

    new HistogramPlotInterface(scores, labels, f).run()
  }

  private def countOrientations(pairs: Seq[(String, String)]): Double = {
    val groupedByValueSet = pairs.groupBy {
      case (src, tgt) => Set(src, tgt)
    }
    val cardinalities = groupedByValueSet.toSeq.map {
      case (nodeSet, pairs) =>
        assert(pairs.size == pairs.toSet.size)
        pairs.size.toDouble
    }
    MathUtil.mean(cardinalities)
  }

  def shuffleBigrams(
    originalBigrams: Seq[(String, String)]
  ): Seq[(String, String)] = {
    val (sources, targets) = originalBigrams.unzip
    val allNames = sources ++ targets

    val random = new Random()

    val shuffled = random.shuffle(allNames)
    val newSources = shuffled.take(originalBigrams.size)
    val newTargets = shuffled.drop(originalBigrams.size)

    newSources zip newTargets
  }

  def randomBigrams(
    nameUniverse: Set[String], size: Int
  ): Seq[(String, String)] = {
    val random = new Random()

    val indexedNames = nameUniverse.toIndexedSeq
    val n = indexedNames.size

    val sources = (1 to size).map(i => indexedNames(random.nextInt(n)))
    val targets = (1 to size).map(i => indexedNames(random.nextInt(n)))

    sources zip targets
  }

}
