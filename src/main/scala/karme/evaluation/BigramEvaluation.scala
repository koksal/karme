package karme.evaluation

import java.io.File

import com.github.tototoshi.csv.CSVReader
import karme.ArgHandling
import karme.EvalOpts
import karme.evaluation.enrichr.EnrichrPredictionLibrary
import karme.evaluation.enrichr.PredictionEvaluator
import karme.parsing.NamesParser

import scala.util.Random

object BigramEvaluation {

  val NB_RUNS = 7

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)

    val bigrams = parseBigrams(opts.evalOpts.bigramFile.get,
      opts.evalOpts.minBigramScore)

    val nameUniverse = NamesParser(opts.evalOpts.nameUniverseFile.get)
    val bigramNameUniverse = bigrams.flatMap(b => Set(b._1, b._2)).toSet
    println(s"Bigram universe size: ${bigramNameUniverse.size}")

    evaluate(bigrams, nameUniverse, opts.evalOpts)

    println("Shuffled bigrams:")
    for (i <- 1 to NB_RUNS) {
      println(s"Run $i:")
      evaluate(shuffleBigrams(bigrams), nameUniverse, opts.evalOpts)
    }

    println("Random bigrams from bigram name universe:")
    for (i <- 1 to NB_RUNS) {
      println(s"Run $i:")
      evaluate(randomBigrams(bigramNameUniverse, bigrams.size),
        nameUniverse, opts.evalOpts)
    }

    println("Random bigrams from full universe:")
    for (i <- 1 to NB_RUNS) {
      println(s"Run $i:")
      evaluate(randomBigrams(nameUniverse, bigrams.size), nameUniverse,
        opts.evalOpts)
    }

  }

  def parseBigrams(f: File, minScore: Int): Seq[(String, String)] = {
    val reader = CSVReader.open(f)
    reader.all() collect {
      case List(src, tgt, score) if score.toInt >= minScore => {
        (src, tgt)
      }
    }
  }

  def evaluate(
    bigrams: Seq[(String, String)],
    nameUniverse: Set[String],
    evalOpts: EvalOpts
  ): Unit = {
    val evalCtx = EvaluationContext.fromOptions(evalOpts)

    for (library <- evalCtx.references) {
      evaluate(bigrams, library, nameUniverse)
    }
  }

  def evaluate(
    bigrams: Seq[(String, String)],
    library: EnrichrPredictionLibrary,
    nameUniverse: Set[String]
  ): Unit = {
    val libraryPairs = PredictionEvaluator.referencePairs(library)

    val score = PredictionSignificanceTest.computeSignificance(
      bigrams.toSet, libraryPairs, nameUniverse)

    println(s"Testing ${bigrams.size} predictions: ${library.id}, $score")
  }

  def shuffleBigrams(
    originalBigrams: Seq[(String, String)]
  ): Seq[(String, String)] = {
    val (sources, targets) = originalBigrams.unzip
    val allNames = sources ++ targets

    val random = new Random()

    val newSources = random.shuffle(allNames).take(originalBigrams.size)
    val newTargets = random.shuffle(allNames).take(originalBigrams.size)

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
