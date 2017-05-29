package karme.evaluation

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import karme.Clustering
import karme.EvalOpts
import karme.FunIOPairsPrediction
import karme.PrecedencePairsPrediction
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

  def evaluatePredictions(): Unit = {
    val predictions = evalOpts.predictionType match {
      case FunIOPairsPrediction =>
        aggregateGeneIOPairs(runDataCollection)
      case PrecedencePairsPrediction =>
        aggregateGeneLevelPrecedences(runDataCollection)
    }

    for (ref <- references) {
      evaluatePairs(predictions, ref)
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
    // val normalizedPredictions = normalizePredictions(predictionsWithCounts)
    val predictionsToEvaluate = predictionsWithCounts

    val (backgroundSources, backgroundTargets) =
      PairEvaluator.edgeSpaceForRunReferenceUnion(predictionsToEvaluate,
        library.ioPairs)

    var predictionsInBackground = PairEvaluator.filterTriplesForNameUniverse(
      predictionsToEvaluate, backgroundSources, backgroundTargets)
    val referenceEdgesInBackground = PairEvaluator.filterPairsForNameUniverse(
      library.ioPairs, backgroundSources, backgroundTargets)

    if (evalOpts.randomize) {
      println("Randomizing predictions.")
      predictionsInBackground = PairEvaluator.randomPredictionsWithUniqueScore(
        predictionsInBackground.size, backgroundSources, backgroundTargets)
    }

    println(s"# non-filtered predictions: ${predictionsToEvaluate.size}")
    println(s"# filtered predictions: ${predictionsInBackground.size}")
    println(s"# non-filtered reference edges: ${library.ioPairs.size}")
    println(s"# filtered reference edges: ${referenceEdgesInBackground.size}")

    new ThresholdedEvaluation(reporter).evaluate(predictionsInBackground,
      referenceEdgesInBackground, backgroundSources, backgroundTargets,
      library.id)

    new PRAUCEvaluation(reporter).evaluate(predictionsInBackground,
      referenceEdgesInBackground, backgroundSources, backgroundTargets,
      library.id)

    findEdgeCoverageRatios(referenceEdgesInBackground.toSeq, library.id)
    findMedianDistances(referenceEdgesInBackground.toSeq, library.id)

    joinPredictionsWithReference(predictionsInBackground,
      referenceEdgesInBackground,
      reporter.file(s"predictions-joined-with-${library.id}"))
  }

  def sourceTargetOverlapCheck(library: EnrichrPredictionLibrary) = {
    val sources = library.ioPairs.map(_._1)
    val targets = library.ioPairs.map(_._2)
    val common = sources intersect targets
    println(s"Source-target overlap check for ${library.id}")
    println(s"Common to reference sources and targets: ${common.size}")
  }

  def transitiveReferenceCheck(library: EnrichrPredictionLibrary) = {
    val tc = transitiveClosure(library.ioPairs)
    val missingFromOriginal = tc -- library.ioPairs
    println(s"Transitive reference check for ${library.id}")
    println("Transitive closure edges missing from reference:")
    println(missingFromOriginal.size)
  }

  def transitiveClosure(pairs: Set[(String, String)]) = {
    var closure = pairs
    var prevClosure = pairs
    do {
      prevClosure = closure
      closure = oneTransitiveStep(closure)
    } while (closure != prevClosure)
    closure
  }

  def oneTransitiveStep(pairs: Set[(String, String)]) = {
    val sources = pairs.map(_._1)
    val edgesToSources = pairs.filter(p => sources.contains(p._2))
    val newEdges = for {
      (src, tgt) <- edgesToSources
      (src2, tgt2) <- pairs.filter(_._1 == tgt)
    } yield {
      val newEdge = (src, tgt2)
      if (!pairs.contains(newEdge)) {
        println(s"Edge in transitive closure, missing: $src -> $tgt -> $tgt2")
      }
      newEdge
    }
    pairs ++ newEdges
  }

  def normalizePredictions(
    predictions: Seq[ScoredPrediction]
  ): Seq[ScoredPrediction] = {
    // for each prediction, normalize score by how often the pair appeared in
    // runs.
    val normalized = for (((src, tgt), score) <- predictions) yield {
      val nbOccurrences = nbRunsWithBothGenes(src, tgt)
      val occurrenceRatio = nbOccurrences.toDouble / runDataCollection.size
      val normalizedScore = score * (1.0 / occurrenceRatio)
      ((src, tgt), normalizedScore.toInt)
    }

    normalized.sortBy(_._2).reverse
  }

  def joinPredictionsWithReference(
    predictions: Seq[ScoredPrediction],
    referenceEdges: Set[(String, String)],
    f: File
  ): Unit = {
    val headers = List("source", "target", "score", "in reference",
      "conflicts reference", "targets in ref.")
    val rows = for (((src, tgt), score) <- predictions) yield {
      val isInReference = if (referenceEdges.contains((src, tgt))) {
        "YES"
      } else {
        ""
      }
      val isConflicting = if (referenceEdges.contains((tgt, src))) {
        "YES"
      } else {
        ""
      }
      val otherTargets = referenceEdges.filter(_._1 == src).map(_._2)
      List(src, tgt, score, isInReference, isConflicting,
        otherTargets.mkString(", "))
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

  def nbRunsWithBothGenes(src: String, tgt: String): Int = {
    val runsWithSrcAndTarget = runDataCollection filter {
      case RunData(_, clustering, _, _) => {
        clustering.allMembers.contains(src) &&
          clustering.allMembers.contains(tgt)
      }
    }
    runsWithSrcAndTarget.size
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

  def edgeSpaceForRunReferenceUnion(
    predictions: Seq[ScoredPrediction],
    referencePairs: Set[(String, String)]
  ): (Set[String], Set[String]) = {
    val predictionNameUnion = PairEvaluator.namesInPairs(predictions.map(_._1))

    val refSources = referencePairs.map(_._1)
    val refTargets = referencePairs.map(_._2)
    val refNames = refSources union refTargets

    val common = predictionNameUnion intersect refNames
    (common, common)
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

    println(s"|G| = ${predictionNames.size}")
    println(s"|G x G| = ${predictionNames.size * predictionNames.size}")
    println(s"|S| = ${refSources.size}")
    println(s"|T| = ${refTargets.size}")
    println(s"|S x T| = ${refSources.size * refTargets.size}")

    val backgroundSources = refSources intersect predictionNames
    val backgroundTargets = refTargets intersect predictionNames

    println(s"|G && S x G && T| = ${backgroundSources.size *
      backgroundTargets.size}")
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

    random.shuffle(res.toSeq)
  }

  def randomPredictionsWithGivenScores(
    scores: Seq[Int],
    sourceUniv: Set[String],
    targetUniv: Set[String]
  ): Seq[ScoredPrediction] = {
    val randomizedPairs = randomPairsWithoutReplacement(sourceUniv,
      targetUniv, scores.size)

    randomizedPairs.zip(scores)
  }

  def randomPredictionsWithUniqueScore(
    size: Int,
    sourceUniv: Set[String],
    targetUniv: Set[String]
  ): Seq[ScoredPrediction] = {
    val randomizedPairs = randomPairsWithoutReplacement(sourceUniv,
      targetUniv, size)

    val scores = (1 to randomizedPairs.size).reverse

    randomizedPairs.zip(scores)
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
