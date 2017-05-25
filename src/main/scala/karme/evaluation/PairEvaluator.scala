package karme.evaluation

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import karme.Clustering
import karme.EvalOpts
import karme.Reporter
import karme.evaluation.Evaluation.ScoredPrediction
import karme.evaluation.enrichr.EnrichrPredictionLibrary
import karme.parsing.IOPairParser
import karme.store.ClusteringStore
import karme.store.EdgePrecedenceStore
import karme.transformations.EdgePrecedence
import karme.util.CollectionUtil
import karme.util.MathUtil
import karme.visualization.HistogramPlotInterface

import scala.util.Random

class PairEvaluator(
  folders: Seq[File],
  references: Seq[EnrichrPredictionLibrary],
  evalOpts: EvalOpts,
  reporter: Reporter
) {

  lazy val runDataCollection: Seq[RunData] = readRuns(folders)

  case class RunData(
    id: String,
    clustering: Clustering,
    precedences: Seq[EdgePrecedence],
    geneIOPairs: Seq[ScoredPrediction]
  )

  def evaluatePrecedences(): Unit = {
    println("Evaluating precedences.")

    val precedencePredictions = aggregateGeneLevelPrecedences(runDataCollection)

    for (ref <- references) {
      evaluatePairs(precedencePredictions, ref)
    }
  }

  def evaluateFunctionIOPairs(): Unit = {
    println("Evaluating function IO pairs.")

    val ioPairPredictions = aggregateGeneIOPairs(runDataCollection)

    for (ref <- references) {
      evaluatePairs(ioPairPredictions, ref)
    }
  }

  def aggregateGeneIOPairs(
    runData: Seq[RunData]
  ): Seq[ScoredPrediction] = {
    val allPairs = runData flatMap {
      case RunData(_, _, _, geneIOPairs) => geneIOPairs
    }

    CollectionUtil.combineCounts(allPairs)
  }

  def aggregateGeneLevelPrecedences(
    runData: Seq[RunData]
  ): Seq[ScoredPrediction] = {
    val expandedPrecedences = runData flatMap {
      case RunData(_, clustering, precedences, _) =>
        precedences flatMap (p => expandEdgePrecedence(p, clustering).toSeq)
    }

    val distanceLimitedPrecedences = limitByMaxDistance(expandedPrecedences)

    val pairs = distanceLimitedPrecedences map {
      case EdgePrecedence(src, tgt, _) => (src, tgt)
    }

    CollectionUtil.orderByCount(pairs)
  }

  def limitByMaxDistance(ps: Seq[EdgePrecedence]): Seq[EdgePrecedence] = {
    ps filter {
      case EdgePrecedence(_, _, dist) =>
        evalOpts.maxPrecedenceDistance match {
          case Some(maxDist) => dist <= maxDist
          case None => true
        }
    }
  }

  def expandEdgePrecedence(
    edgePrecedence: EdgePrecedence, clustering: Clustering
  ): Set[EdgePrecedence] = {
    for {
      src <- clustering.clusterToMember(edgePrecedence.source)
      tgt <- clustering.clusterToMember(edgePrecedence.target)
    } yield {
      EdgePrecedence(src, tgt, edgePrecedence.distance)
    }
  }

  def aggregateClusteredNames(clusterings: Seq[Clustering]): Set[String] = {
    clusterings.map(_.allMembers).flatten.toSet
  }

  def readRuns(folders: Seq[File]): Seq[RunData] = {
    println(s"Reading run data from ${folders.size} folders.")

    folders map { f =>
      val clustering = Clustering(new ClusteringStore(f).read)
      val precedences = new EdgePrecedenceStore(f).read
      val geneIOPairs = IOPairParser(new File(f, "gene-io-pairs.csv"))

      val nonSelfPrecedences = precedences filter (p => p.source != p.target)

      RunData(f.getName, clustering, nonSelfPrecedences, geneIOPairs)
    }
  }

  def evaluatePairs(
    predictionsWithCounts: Seq[ScoredPrediction],
    library: EnrichrPredictionLibrary
  ): Unit = {
    val (backgroundSources, backgroundTargets) =
      PairEvaluator.edgeSpaceForRunUnion(predictionsWithCounts, library.ioPairs)

    var predictionsInBackground = PairEvaluator.filterTriplesForNameUniverse(
      predictionsWithCounts, backgroundSources, backgroundTargets)
    val referenceEdgesInBackground = PairEvaluator.filterPairsForNameUniverse(
      library.ioPairs, backgroundSources, backgroundTargets)

    if (evalOpts.randomize) {
      println("Randomizing predictions.")
      predictionsInBackground = PairEvaluator.randomPredictionsWithSameScore(
        predictionsInBackground, backgroundSources, backgroundTargets)
    }

    println(s"# predictions: ${predictionsInBackground.size}")
    println(s"# reference edges: ${referenceEdgesInBackground.size}")

    // new ThresholdedEvaluation(reporter).evaluate(predictionsInBackground,
    //   referenceEdgesInBackground, backgroundSources, backgroundTargets,
    //   library.id)

    // new PRAUCEvaluation(reporter).evaluate(predictionsInBackground,
    //   referenceEdgesInBackground, backgroundSources, backgroundTargets,
    //   library.id)

    // findEdgeCoverageRatios(referenceEdgesInBackground.toSeq, library.id)
    // findMedianDistances(referenceEdgesInBackground.toSeq, library.id)

    joinPredictionsWithReference(predictionsInBackground,
      referenceEdgesInBackground,
      reporter.file(s"predictions-joined-with-${library.id}"))
  }

  def joinPredictionsWithReference(
    predictions: Seq[ScoredPrediction],
    referenceEdges: Set[(String, String)],
    f: File
  ): Unit = {
    val headers = List("source", "target", "score", "in reference",
      "targets in ref.")
    val rows = for (((src, tgt), score) <- predictions) yield {
      val isInReference = if (referenceEdges.contains((src, tgt))) {
        "YES"
      } else {
        ""
      }
      val otherTargets = referenceEdges.filter(_._1 == src).map(_._2)
      List(src, tgt, score, isInReference, otherTargets.mkString(", "))
    }

    val writer = CSVWriter.open(f)
    writer.writeAll(headers +: rows)
    writer.close
  }

  def findEdgeCoverageRatios(
    refPairs: Seq[(String, String)],
    refID: String
  ): Unit = {
    val coverageRatios = for ((src, tgt) <- refPairs) yield {
      // find runs that have both names, by checking clustering.
      val runsWithSrcAndTarget = runDataCollection filter {
        case RunData(_, clustering, _, _) => {
          clustering.allMembers.contains(src) &&
            clustering.allMembers.contains(tgt)
        }
      }
      val runsWithSameSrcTgtCluster = runsWithSrcAndTarget filter {
        case RunData(_, clustering, _, _) => {
          clustering.memberToCluster(src) == clustering.memberToCluster(tgt)
        }
      }
      val coverageRatio =
        runsWithSrcAndTarget.size.toDouble / runDataCollection.size

      val coClusteringRatio =
        runsWithSameSrcTgtCluster.size.toDouble / runsWithSrcAndTarget.size

      (coverageRatio, coClusteringRatio)
    }

    val coverageLabels = coverageRatios.map(_ => "coverage ratio")
    val coClusteringLabels = coverageRatios.map(_ => "co-clustering ratio")
    new HistogramPlotInterface(coverageRatios.map(_._1), coverageLabels,
      reporter.file(s"coverage-ratio-${refID}.pdf")).run()
    new HistogramPlotInterface(coverageRatios.map(_._2), coClusteringLabels,
      reporter.file(s"same-cluster-ratio-${refID}.pdf")).run()
  }

  def findMedianDistances(
    refPairs: Seq[(String, String)],
    refID: String
  ): Unit = {
    val refPrecedences = refPairs.map{
      case (src, tgt) => {
        runDataCollection flatMap {
          case RunData(_, clustering, precedences, _) => {
            val srcClusterOpt = clustering.memberToCluster.get(src)
            val tgtClusterOpt = clustering.memberToCluster.get(tgt)
            (srcClusterOpt, tgtClusterOpt) match {
              case (Some(srcClust), Some(tgtClust)) => {
                precedences.filter(
                  p => p.source == srcClust && p.target == tgtClust)
              }
              case _ => {
                Nil
              }
            }
          }
        }
      }
    }.filter(_.nonEmpty)

    val medianDistances = refPrecedences.map(
      ps => MathUtil.median(ps.map(_.distance)))
    val labels = medianDistances map (_ => "median distance")
    new HistogramPlotInterface(medianDistances, labels,
      reporter.file(
        s"reference-edge-median-precedence-distance-distribution-" +
          s"${refID}.pdf")).run()
  }

  def namesCommonToAllRuns: Set[String] = {
    val nameSets = runDataCollection.map(_.clustering.allMembers)

    val commonNames = nameSets.reduceLeft[Set[String]] {
      case (acc, ns) => acc.intersect(ns)
    }

    println(s"# names common to all runs: ${commonNames.size}")

    commonNames
  }

  def plotAllDistances(): Unit = {
    val distances = runDataCollection.flatMap(_.precedences).map(_.distance)
    println(s"Found ${distances.size} distances.")

    val labels = distances map (_ => "distance")

    new HistogramPlotInterface(distances, labels,
      reporter.file(
        s"all-distances-distribution.pdf")).run()
  }
}

object PairEvaluator {

  def edgeSpaceForRunIntersection(
    namesCommonToRuns: Set[String],
    referencePairs: Set[(String, String)]
  ): (Set[String], Set[String]) = {
    edgeSpace(namesCommonToRuns, referencePairs)
  }

  def edgeSpaceForRunUnion(
    predictions: Seq[ScoredPrediction],
    referencePairs: Set[(String, String)]
  ): (Set[String], Set[String]) = {
    val predictionNameUnion = PairEvaluator.namesInPairs(predictions.map(_._1))
    edgeSpace(predictionNameUnion, referencePairs)
  }

  def edgeSpace(
    predictionNames: Set[String],
    referencePairs: Set[(String, String)]
  ): (Set[String], Set[String]) = {
    val refSources = referencePairs.map(_._1)
    val refTargets = referencePairs.map(_._2)

    val backgroundSources = refSources intersect predictionNames
    val backgroundTargets = refTargets intersect predictionNames

    (backgroundSources, backgroundTargets)
  }

  def plotScoreDist(scores: Seq[Double], f: File): Unit = {
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
    sourceUniverse: Set[String],
    targetUniverse: Set[String],
    size: Int
  ): Seq[(String, String)] = {
    val random = new Random()

    val indexedSources = sourceUniverse.toIndexedSeq
    val indexedTargets = targetUniverse.toIndexedSeq

    var res = Set[(String, String)]()
    while (res.size < size) {
      res +=
        ((indexedSources(random.nextInt(indexedSources.size)),
          indexedTargets(random.nextInt(indexedTargets.size))))
    }

    res.toSeq
  }

  def randomPredictionsWithSameScore(
    predictions: Seq[ScoredPrediction],
    sourceUniv: Set[String],
    targetUniv: Set[String]
  ): Seq[ScoredPrediction] = {
    val randomizedPairs = randomPairsWithoutReplacement(sourceUniv,
      targetUniv, predictions.size)

    predictions.zip(randomizedPairs) map {
      case (origPrediction, randomPair) => (randomPair, origPrediction._2)
    }
  }

  def randomPredictionsWithUniqueScore(
    predictions: Seq[ScoredPrediction],
    sourceUniv: Set[String],
    targetUniv: Set[String]
  ): Seq[ScoredPrediction] = {
    val randomizedPairs = randomPairsWithoutReplacement(sourceUniv,
      targetUniv, predictions.size)

    randomizedPairs.zipWithIndex
  }

  def namesInPairs(pairs: Iterable[(String, String)]): Set[String] = {
    pairs.toSet[(String, String)] flatMap {
      case (src, tgt) => Set(src, tgt)
    }
  }

  def filterPairsForNameUniverse(
    pairs: Set[(String, String)],
    possibleSources: Set[String],
    possibleTargets: Set[String]
  ): Set[(String, String)] = {
    pairs filter {
      case (src, tgt) =>
        possibleSources.contains(src) && possibleTargets.contains(tgt)
    }
  }

  def filterTriplesForNameUniverse(
    pairs: Seq[ScoredPrediction],
    possibleSources: Set[String],
    possibleTargets: Set[String]
  ): Seq[ScoredPrediction] = {
    pairs filter {
      case ((src, tgt), i) =>
        possibleSources.contains(src) && possibleTargets.contains(tgt)
    }
  }

}
