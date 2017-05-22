package karme.evaluation

import java.io.File

import karme.Clustering
import karme.EvalOpts
import karme.Reporter
import karme.evaluation.Evaluation.ScoredPrediction
import karme.evaluation.enrichr.EnrichrPredictionLibrary
import karme.store.ClusteringStore
import karme.store.EdgePrecedenceStore
import karme.transformations.EdgePrecedence
import karme.util.CollectionUtil
import karme.util.MathUtil
import karme.visualization.HistogramPlotInterface

import scala.util.Random

class PairEvaluator(
  references: Seq[EnrichrPredictionLibrary],
  evalOpts: EvalOpts,
  reporter: Reporter
) {

  def evaluatePrecedences(folders: Seq[File]): Unit = {
    val runData = readRuns(folders)

    val precedencePredictions = aggregateGeneLevelPrecedences(runData)

    for (ref <- references) {
      evaluatePairs(precedencePredictions, ref)
      // analyzeRunCollection(runData, ref)
    }
  }

  def analyzeRunCollection(
    runData: Seq[(Clustering, Seq[EdgePrecedence])],
    ref: EnrichrPredictionLibrary
  ): Unit = {
    println(s"Analyzing ${ref.id}")

    val namesInClusterings = aggregateClusteredNames(runData.map(_._1))

    val (edgesWithKnownGenes, edgesWithUnknownGenes) = ref.ioPairs.partition {
      case (src, tgt) =>
        namesInClusterings.contains(src) && namesInClusterings.contains(tgt)
    }

    println(s"Reference edges with known genes: ${edgesWithKnownGenes.size}")
    println(
      s"Reference edges with unknown genes: ${edgesWithUnknownGenes.size}")

    for (e <- edgesWithKnownGenes) {
      analyzeReferenceEdge(e, runData)
    }
  }

  def analyzeReferenceEdge(
    edge: (String, String),
    runData: Seq[(Clustering, Seq[EdgePrecedence])]
  ): Unit = {
    // for each run:
    //   is the edge in the clustering?
    //   is it within a cluster?
    //   what is the distribution of distances for the edge?

    var nbHits = 0
    var nbMisses = 0

    var nbSharedClusterRuns = 0
    var matchingPrecedences = Seq[EdgePrecedence]()

    for ((clustering, precedences) <- runData) {
      val lhsClustOpt = clustering.memberToCluster.get(edge._1)
      val rhsClustOpt = clustering.memberToCluster.get(edge._2)

      (lhsClustOpt, rhsClustOpt) match {
        case (Some(lhsClust), Some(rhsClust)) => {
          nbHits += 1
          if (lhsClust == rhsClust) {
            nbSharedClusterRuns += 1
          }

          val matching = precedences filter {
            case EdgePrecedence(src, tgt, dist) => {
              src == lhsClust && tgt == rhsClust
            }
          }

          matchingPrecedences = matchingPrecedences ++ matching
        }
        case _ => {
          nbMisses += 1
        }
      }
    }

    println(s"Analyzing edge: $edge")
    println(s"Nb hits: $nbHits")
    println(s"Nb misses: $nbMisses")
    println(s"Nb shared cluster runs: $nbSharedClusterRuns")
    println(s"Distances in graphs:")
    println(matchingPrecedences.mkString("\n"))

  }

  def aggregateGeneLevelPrecedences(
    runData: Seq[(Clustering, Seq[EdgePrecedence])]
  ): Seq[ScoredPrediction] = {
    val expandedPrecedences = runData flatMap {
      case (clus, precs) =>
        precs flatMap (p => expandEdgePrecedence(p, clus).toSeq)
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

  def readRuns(folders: Seq[File]): Seq[(Clustering, Seq[EdgePrecedence])] = {
    println(s"Reading run data from ${folders.size} folders.")

    folders map { f =>
      val clustering = Clustering(new ClusteringStore(f).read)
      val precedences = new EdgePrecedenceStore(f).read

      val nonSelfPrecedences = precedences filter (p => p.source != p.target)
      (clustering, nonSelfPrecedences)
    }
  }

  def evaluatePairs(
    predictionsWithCounts: Seq[ScoredPrediction],
    library: EnrichrPredictionLibrary
  ): Unit = {
    new HypergeometricEvaluation(reporter).evaluate(predictionsWithCounts,
      library)

    new PRAUCEvaluation(reporter).evaluate(predictionsWithCounts, library)
  }
}

object PairEvaluator {
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

    val randomizedPairs = randomPairsWithoutReplacement(
      namesInPairs(predictions.map(_._1)),
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
