package karme.evaluation

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import karme._
import karme.evaluation.Evaluation.ScoredPrediction
import karme.evaluation.PredictionTypes.{FunIOPairsPrediction, PrecedencePairsPrediction}
import karme.store.ClusteringStore
import karme.transformations.EdgePrecedence
import karme.transformations.RankSumTest
import karme.transformations.clustering.ReferenceClustering
import karme.util.CollectionUtil
import karme.util.FileUtil
import karme.util.MathUtil
import karme.visualization.HistogramPlotInterface

import scala.util.Random

class PairEvaluator(
  folders: Seq[File],
  references: Seq[PredictionLibrary],
  evalOpts: EvalOpts,
  reporter: Reporter
) {

  lazy val runDataCollection: Seq[RunData] = readRuns(folders)

  val histogramPlotInterface = new HistogramPlotInterface()

  case class RunData(
    id: String,
    clustering: Clustering,
    precedences: Seq[EdgePrecedence],
    geneIOPairs: Seq[ScoredPrediction]
  )

  def evaluateClusterings(): Unit = {
    for (reference <- references) {
      val refClustering =
        ReferenceClustering.clusterTargetsByCommonResponse(reference)

      // TODO plot clustering information and maybe plot per pseudotime
      println(s"Found ${refClustering.allClusters.size} clusters.")

      for (runData <- runDataCollection) {
        // evaluateClustering(runData.id, runData.clustering, reference)
        evaluateClusterSimilarity(refClustering, runData.clustering)
      }
    }
  }

  def evaluateClusterSimilarity(
    referenceGeneClustering: Clustering,
    singleCellGeneClustering: Clustering
  ): Unit = {
    var jacSimilarities = Set[(String, String, Double)]()

    for {
      refCluster <- referenceGeneClustering.allClusters
      scCluster <- singleCellGeneClustering.allClusters
    } {
      // what is the jaccard similarity between the two
      val jacSimilarity = CollectionUtil.jaccardSimilarity(
        referenceGeneClustering.clusterToMembers(refCluster),
        singleCellGeneClustering.clusterToMembers(scCluster)
      )
      jacSimilarities += ((refCluster, scCluster, jacSimilarity))
    }

    val orderedSimilarities = jacSimilarities.toSeq.sortBy(- _._3)
    for ((refCluster, scCluster, similarity) <- orderedSimilarities) {
      println(s"$refCluster, $scCluster, $similarity")
    }

  }

  def evaluateClustering(
    runID: String, clustering: Clustering, reference: PredictionLibrary
  ): Unit = {
    // TODO filter clustering to names in reference.
    val clusteringFilteredByRef = filterClusteringByReferenceTargets(clustering,
      reference)

    val sourceToReferencePreds = reference.predictions.groupBy(_.source)

    var zeroRatios = List[Double]()
    for {
      (cluster, i) <- clusteringFilteredByRef.allClusters.zipWithIndex
      (refSource, refPredsFromSource) <- sourceToReferencePreds
    } {
      val members = clusteringFilteredByRef.clusterToMembers(cluster)

      if (members.nonEmpty) {
        val fcs = clusterFoldChanges(members, refPredsFromSource)

        // val f = reporter.file(s"cluster-$i-knockdown-$refSource.pdf")
        // histogramPlotInterface.plot(fcs, f)

        val posFcs = fcs.filter(_ > 0)
        val negFcs = fcs.filter(_ < 0)
        val zeroFcs = fcs.filter(_ == 0)
        val zeroRatio = MathUtil.roundTo(4)(zeroFcs.size.toDouble / fcs.size)
        println(
          List(s"cluster-$i", refSource, zeroRatio, fcs.size).mkString(","))
        zeroRatios = zeroRatio :: zeroRatios
      }
    }

    histogramPlotInterface.plot(zeroRatios,
      reporter.file(s"${runID}-zero-ratios-distribution.pdf"))
  }

  def filterClusteringByReferenceTargets(
    clustering: Clustering, reference: PredictionLibrary
  ): Clustering = {
    val referenceTargets = reference.predictions.map(_.target).toSet
    val newMapping = clustering.clusterToMembers map {
      case (cluster, members) => {
        (cluster, members intersect referenceTargets)
      }
    }
    val nonEmptyClusters = newMapping filter {
      case (cluster, members) => members.nonEmpty
    }
    Clustering(nonEmptyClusters)
  }

  // returns missing effects as 0.0
  def clusterFoldChanges(
    cluster: Set[String], preds: Seq[ReferencePrediction]
  ): Seq[Double] = {
    val targetToPreds = preds.groupBy(_.target)
    cluster.toSeq flatMap { member =>
      targetToPreds.get(member) match {
        case Some(predsToTarget) => {
          predsToTarget.map(_.weight)
        }
        case None => Seq(0.0)
      }
    }
  }

  def evaluatePredictions(): Unit = {
    var predictions = evalOpts.predictionType match {
      case FunIOPairsPrediction =>
        aggregateInterClusterGeneIOPairs(runDataCollection)
      case PrecedencePairsPrediction =>
        aggregateGeneLevelPrecedences(runDataCollection)
    }

    if (evalOpts.normalizeScores) {
      predictions = normalizePredictions(predictions)
    }

    for (ref <- references) {
      // evaluatePairs(predictions, ref)
      // evaluateReferenceWeightsForPredictions(predictions, ref)
      oneHopTransitiveCheck(ref);
    }
  }

  def aggregateInterClusterGeneIOPairs(
    runData: Seq[RunData]
  ): Seq[ScoredPrediction] = {
    val interClusterPairs = runData flatMap { rd =>
      rd.geneIOPairs filter {
        case ((src, tgt), _) => {
          rd.clustering.memberToCluster(src) !=
            rd.clustering.memberToCluster (tgt)
        }
      }
    }

    CollectionUtil.combineCounts(interClusterPairs)
  }

  def aggregateGeneLevelPrecedences(
    runData: Seq[RunData]
  ): Seq[ScoredPrediction] = {
    val expandedPrecedences = runData flatMap { rd => rd.precedences }

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

  def aggregateClusteredNames(clusterings: Seq[Clustering]): Set[String] = {
    clusterings.map(_.allMembers).flatten.toSet
  }

  def readRuns(folders: Seq[File]): Seq[RunData] = {
    println(s"Reading run data from ${folders.size} folders.")

    folders map { f =>
      val clustering = Clustering(new ClusteringStore(f).read)

      val runID = FileUtil.getFileName(f.getName)

      RunData(runID, clustering, Nil, Nil)
      /*
      evalOpts.predictionType match {
        case FunIOPairsPrediction => {
          val geneIOPairs = IOPairParser(new File(f, "gene-io-pairs.csv"))
          RunData(runID, clustering, Nil, geneIOPairs)
        }
        case PrecedencePairsPrediction => {
          val precedences = new EdgePrecedenceStore(f).read

          assert(precedences.forall(p => p.source != p.target))

          RunData(runID, clustering, precedences, Nil)
        }
      }
      */
    }
  }

  def evaluateReferenceWeightsForPredictions(
    scoredPredictions: Seq[ScoredPrediction],
    library: PredictionLibrary
  ) = {
    // optionally compute 1-hop transitive reference predictions
    val useTransitivePredictions = false
    val refPredictions = if (useTransitivePredictions) {
      transitiveReferencePreds(library.predictions)
    } else {
      library.predictions
    }
    val refPairs = refPredictions.map(p => (p.source, p.target))

    // intersect domains
    val (backgroundSources, backgroundTargets) =
      PairEvaluator.sourceTargetProductBackground(scoredPredictions,
        refPairs.toSet)
    val predictionsInBackground =
      PairEvaluator.filterScoredPredictionsForDomain(scoredPredictions,
        backgroundSources, backgroundTargets)
    val refPredsInBackground =
      PairEvaluator.filterReferencePredictionsForDomain(refPredictions,
        backgroundSources, backgroundTargets)

    // compute the reference weights for each predictions
    val weights = absRefWeightsForPredictions(predictionsInBackground,
      refPredsInBackground)
    val nonZeroWeights = weights.filter(_ > 0)
    val weightsToTest = nonZeroWeights

    val rs = new RankSumTest()
    var rsResults = Set[Double]()
    val histPlot = new HistogramPlotInterface()
    for (i <- 0 until 100) {
      // compare to randomly selected predictions from domain
      /*
      val randomPreds = PairEvaluator.randomPredictionsWithGivenScores(
        predictionsInBackground.map(_._2), backgroundSources, backgroundTargets)
      val randomWeights = absRefWeightsForPredictions(randomPreds,
        refPredsInBackground)
        */
      val randomWeights = new Random().shuffle(refPredsInBackground).take(
        nonZeroWeights.size).map(p => math.abs(p.weight))

      // plot both and perform distribution comparison
      val fstLabels = weightsToTest map (w => "Original")
      val sndLabels = randomWeights map (w => "Randomized")
      val allPoints = weightsToTest ++ randomWeights
      val allLabels = fstLabels ++ sndLabels
      histPlot.plot(allPoints, allLabels,
        reporter.file(s"randomized-weight-comparison-$i.pdf"))

      val originalGreaterP = rs.testPValue(weightsToTest, randomWeights)
      println(s"Original weights are greater: ${originalGreaterP}")
      rsResults += originalGreaterP
    }

    println(s"Average p-value: ${MathUtil.mean(rsResults)}")
  }

  def absRefWeightsForPredictions(
    predictions: Seq[ScoredPrediction],
    refPredictions: Seq[ReferencePrediction]
  ): Seq[Double] = {
    predictions map (p => absRefWeightForPrediction(p, refPredictions))
  }

  def absRefWeightForPrediction(
    prediction: ScoredPrediction,
    refPredictions: Seq[ReferencePrediction]
  ): Double = {
    // inefficient lookup for now
    val ((predSrc, predTgt), predW) = prediction
    val matchingRefPred = refPredictions.find {
      case ReferencePrediction(term, target, _) => {
        term == predSrc && target == predTgt
      }
    }
    matchingRefPred match {
      case Some(pred) => math.abs(pred.weight)
      case None => 0.0
    }
  }

  def evaluatePairs(
    scoredPredictions: Seq[ScoredPrediction],
    library: PredictionLibrary
  ): Unit = {
    val referencePairs = library.ioPairs

    val (backgroundSources, backgroundTargets) =
      PairEvaluator.sourceTargetProductBackground(scoredPredictions,
        referencePairs)

    var predictionsInBackground =
      PairEvaluator.filterScoredPredictionsForDomain(scoredPredictions,
        backgroundSources, backgroundTargets)
    val referenceEdgesInBackground = PairEvaluator.filterPairsForDomain(
      referencePairs, backgroundSources, backgroundTargets)

    if (evalOpts.randomize) {
      println("Randomizing predictions.")
      predictionsInBackground = PairEvaluator.randomPredictionsWithUniqueScore(
        predictionsInBackground.size, backgroundSources, backgroundTargets)
    }

    println(s"# non-filtered predictions: ${scoredPredictions.size}")
    println(s"# filtered predictions: ${predictionsInBackground.size}")
    println(s"# non-filtered reference edges: ${referencePairs.size}")
    println(s"# filtered reference edges: ${referenceEdgesInBackground.size}")

    new ThresholdedEvaluation(reporter).evaluate(predictionsInBackground,
      referenceEdgesInBackground, backgroundSources, backgroundTargets,
      library.id)

    new PRAUCEvaluation(reporter).evaluate(predictionsInBackground,
      referenceEdgesInBackground, backgroundSources, backgroundTargets,
      library.id)

    // findEdgeCoverageRatios(referenceEdgesInBackground.toSeq, library.id)
    // findMedianDistances(referenceEdgesInBackground.toSeq, library.id)

    joinPredictionsWithReference(predictionsInBackground,
      referenceEdgesInBackground,
      reporter.file(s"predictions-joined-with-${library.id}"))
  }

  def sourceTargetOverlapCheck(library: PredictionLibrary) = {
    val sources = library.ioPairs.map(_._1)
    val targets = library.ioPairs.map(_._2)
    val common = sources intersect targets
    println(s"Source-target overlap check for ${library.id}")
    println(s"Common to reference sources and targets: ${common.size}")
  }

  def transitiveReferenceCheck(library: PredictionLibrary) = {
    val tc = transitiveClosure(library.ioPairs)
    val missingFromOriginal = tc -- library.ioPairs
    println(s"Transitive reference check for ${library.id}")
    println("Transitive closure edges missing from reference:")
    println(missingFromOriginal.size)
  }

  def oneHopTransitiveCheck(library: PredictionLibrary) = {
    val termToPredictions = library.predictions.groupBy(p => p.source)

    var missed = 0
    var captured = 0

    var missedMinFoldChanges = List[Double]()
    var capturedMinFoldChanges = List[Double]()

    for (firstHop <- library.predictions;
         if firstHop.weight < 0 && firstHop.source != firstHop.target) {
      val predictionsFromSource =
        termToPredictions.getOrElse(firstHop.source, Nil)
      val predictionsFromTarget =
        termToPredictions.getOrElse(firstHop.target, Nil)

      for (secondHop <- predictionsFromTarget;
           if secondHop.source != secondHop.target) {

        val minFoldChange = math.min(
          math.abs(firstHop.weight),
          math.abs(secondHop.weight)
        )

        predictionsFromSource.find(
          p => p.target == secondHop.target) match {
          case Some(twoHop) => {
            captured += 1
            capturedMinFoldChanges = minFoldChange :: capturedMinFoldChanges
          }
          case None => {
            println(s"Reference predicts: $firstHop")
            println(s"Reference also predicts: $secondHop")
            println(s"Reference has no edge from ${firstHop.source} to " +
              s"${secondHop.target}")
            println

            missed += 1
            missedMinFoldChanges = minFoldChange :: missedMinFoldChanges
          }

        }
      }
    }

    println(s"Missed 1-hop connections: $missed")
    println(s"Captured 1-hop connections: $captured")

    val points = capturedMinFoldChanges ::: missedMinFoldChanges
    val labels = capturedMinFoldChanges.map(_ => "captured") :::
      missedMinFoldChanges.map(_ => "missed")
    new HistogramPlotInterface().plot(
      points,
      labels,
      reporter.file(s"transitive-abs-fold-changes-${library.id}.pdf")
    )

    val rs = new RankSumTest().test(
      capturedMinFoldChanges, missedMinFoldChanges)
    println(rs)

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

  def transitiveReferencePreds(
    referencePredictions: Seq[ReferencePrediction]
  ) = {
    // group by source, target
    val srcToPred = referencePredictions.groupBy(_.source)
    val pairs = referencePredictions.map(p => (p.source, p.target)).toSet

    var transitivePreds = Set[ReferencePrediction]()

    for (firstPred <- referencePredictions) {
      val predsFromTarget = srcToPred.getOrElse(firstPred.target, Nil)
      for (secondPred <- predsFromTarget) {
        val transitivePair = (firstPred.source, secondPred.target)
        if (!pairs.contains(transitivePair)) {
          val transitiveWeight = math.min(firstPred.weight, secondPred.weight)
          val transitivePred = ReferencePrediction(transitivePair._1,
            transitivePair._2, transitiveWeight)
          transitivePreds += transitivePred
        }
      }
    }

    println(s"Ref preds: ${referencePredictions.size}")
    println(s"Added transitive preds: ${transitivePreds.size}")
    referencePredictions ++ transitivePreds.toSeq
  }

  def oneTransitiveStep(pairs: Set[(String, String)]) = {
    val sources = pairs.map(_._1)
    val edgesToSources = pairs.filter(p => sources.contains(p._2))
    val newEdges = for {
      (src, tgt) <- edgesToSources
      (src2, tgt2) <- pairs.filter(_._1 == tgt)
    } yield {
      val newEdge = (src, tgt2)
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
    new HistogramPlotInterface().plot( coverageRatios.map(_._1), coverageLabels,
      reporter.file(s"coverage-ratio-${refID}.pdf"))
    new HistogramPlotInterface().plot(coverageRatios.map(_._2),
      coClusteringLabels, reporter.file(s"same-cluster-ratio-${refID}.pdf"))
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
    new HistogramPlotInterface().plot(medianDistances, labels,
      reporter.file(s"reference-edge-median-precedence-distance-distribution-" +
          s"${refID}.pdf"))
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

    new HistogramPlotInterface().plot(distances, labels,
      reporter.file(s"all-distances-distribution.pdf"))
  }
}

object PairEvaluator {

  def edgeSpaceForRunIntersection(
    namesCommonToRuns: Set[String],
    referencePairs: Set[(String, String)]
  ): (Set[String], Set[String]) = {
    edgeSpace(namesCommonToRuns, referencePairs)
  }

  def sourceTargetUnionBackground(
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

  def sourceTargetProductBackground(
    predictions: Seq[ScoredPrediction],
    referencePairs: Set[(String, String)]
  ): (Set[String], Set[String]) = {
    val predictionNameUnion = PairEvaluator.namesInPairs(predictions.map(_._1))
    edgeSpace(predictionNameUnion, referencePairs)
  }

  def referenceSourceTargetProductSpace(
    referencePairs: Set[(String, String)]
  ): (Set[String], Set[String]) = {
    val refSources = referencePairs.map(_._1)
    val refTargets = referencePairs.map(_._2)

    (refSources, refTargets)
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

  def filterReferencePredictionsForDomain(
    predictions: Seq[ReferencePrediction],
    possibleSources: Set[String],
    possibleTargets: Set[String]
  ): Seq[ReferencePrediction] = {
    predictions filter {
      case ReferencePrediction(term, target, _) => {
        possibleSources.contains(term) && possibleTargets.contains(target)
      }
    }
  }

  def filterPairsForDomain(
    pairs: Set[(String, String)],
    possibleSources: Set[String],
    possibleTargets: Set[String]
  ): Set[(String, String)] = {
    pairs filter {
      case (src, tgt) =>
        possibleSources.contains(src) && possibleTargets.contains(tgt)
    }
  }

  def filterScoredPredictionsForDomain(
    predictions: Seq[ScoredPrediction],
    possibleSources: Set[String],
    possibleTargets: Set[String]
  ): Seq[ScoredPrediction] = {
    predictions filter {
      case ((src, tgt), i) =>
        possibleSources.contains(src) && possibleTargets.contains(tgt)
    }
  }

}
