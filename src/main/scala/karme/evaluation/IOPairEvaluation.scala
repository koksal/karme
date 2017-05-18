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

  type ScoredPrediction = ((String, String), Int)

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
    predictionsWithCounts: Seq[ScoredPrediction],
    library: EnrichrPredictionLibrary,
    reporter: Reporter
  ): Unit = {
    val hypergeomEval = new HypergeometricEvaluation(reporter)
    val randomized = randomPredictionsWithSameScore(predictionsWithCounts)
    hypergeomEval.evaluate(predictionsWithCounts, library)

    // new PRAUCEvaluation(reporter).evaluate(predictionsWithCounts, library)
  }


  private def plotScoreDist(scores: Seq[Double], f: File): Unit = {
    val labels = scores.map(_ => "none")

    new HistogramPlotInterface(scores, labels, f).run()
  }

  def meanOrientationCardinality(pairs: Seq[(String, String)]): Double = {
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

  def shufflePairs(
    originalPairs: Seq[(String, String)]
  ): Seq[(String, String)] = {
    val (sources, targets) = originalPairs.unzip
    val allNames = sources ++ targets

    val random = new Random()

    val shuffled = random.shuffle(allNames)
    val newSources = shuffled.take(originalPairs.size)
    val newTargets = shuffled.drop(originalPairs.size)

    newSources zip newTargets
  }

  def randomPairs(
    nameUniverse: Set[String], size: Int
  ): Seq[(String, String)] = {
    val random = new Random()

    val indexedNames = nameUniverse.toIndexedSeq
    val n = indexedNames.size

    val sources = (1 to size).map(i => indexedNames(random.nextInt(n)))
    val targets = (1 to size).map(i => indexedNames(random.nextInt(n)))

    sources zip targets
  }

  def randomPairsWithoutReplacement(
    nameUniverse: Set[String], size: Int
  ): Seq[(String, String)] = {
    val random = new Random()

    val indexedNames = nameUniverse.toIndexedSeq
    val n = indexedNames.size

    var res = Set[(String, String)]()
    while (res.size < size) {
      res +=
        ((indexedNames(random.nextInt(n)), indexedNames(random.nextInt(n))))
    }

    res.toSeq
  }

  def randomPredictionsWithSameScore(
    predictions: Seq[ScoredPrediction]
  ): Seq[ScoredPrediction] = {

    val randomizedPairs = IOPairEvaluation.randomPairsWithoutReplacement(
      IOPairEvaluation.namesInPairs(predictions.map(_._1)),
      predictions.size).toSet

    predictions.zip(randomizedPairs) map {
      case (pred, pair) => (pair, pred._2)
    }
  }

  def namesInPairs(pairs: Iterable[(String, String)]): Set[String] = {
    pairs.toSet[(String, String)] flatMap {
      case (src, tgt) => Set(src, tgt)
    }
  }
}
