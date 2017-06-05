package karme.transformations

import karme.AnnotationContext
import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment
import karme.Experiments.{BooleanExperiment, ContinuousExperiment}
import karme.Reporter
import karme.graphs.Graphs.UnlabeledEdge
import karme.{Experiments, InputTransformerOpts}
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.{DirectedBooleanStateGraph, StateGraphVertex, UndirectedStateGraphOps}
import karme.parsing.{BooleanExperimentParser, CellTrajectoryParser, ContinuousExperimentParser, NamesParser}
import karme.transformations.clustering.HierarchicalClustering
import karme.transformations.discretization.Discretization
import karme.transformations.smoothing.BinomialMLE
import karme.util.NamingUtil
import karme.visualization.CurvePlot

case class TransformResult(
  graph: DirectedBooleanStateGraph,
  sources: Set[StateGraphVertex],
  clustering: Map[String, Set[String]]
)

class InputTransformer(
  opts: InputTransformerOpts,
  annotationContext: AnnotationContext,
  reporter: Reporter
) {

  val trajectories: Seq[CellTrajectory] = {
    opts.inputFileOpts.trajectoryFiles map CellTrajectoryParser.parse
  }

  val geneNamesToFilter: Option[Set[String]] = {
    new NamesParser(opts.inputFileOpts.namesFiles).names
  }

  val inputExperiment: Experiment[Double] = {
    val file = opts.inputFileOpts.continuousExperimentFile.getOrElse(
      sys.error("No continuous experiment given."))
    ContinuousExperimentParser.parseAndFilter(file, geneNamesToFilter)
  }

  def transform(): TransformResult = {
    val smoothedExp = getSmoothedExperiment()

    val geneClustering = HierarchicalClustering.computeBestClustering(
      smoothedExp, opts.clusteringOpts)

    new CurvePlot(reporter).plotClusterCurves(smoothedExp, trajectories,
      geneClustering, "smoothed-experiment")

    val (graph, sources) = graphAndSourcesFromClusterAverages(smoothedExp,
      geneClustering)

    TransformResult(graph, sources, geneClustering)
  }

  def refineClusteringPerEdge(
    geneLevelExp: Experiment[Double],
    transformResult: TransformResult
  ) = {
    // 1. for each edge, label
    //      for each gene in label cluster
    //        test if the gene's value in the smoothed experiment changes
    //        between the two nodes (rank-sum test)

    for (e <- transformResult.graph.E) {
      val refinedClustering = refineClusteringForEdgeLabels(e,
        transformResult.clustering, geneLevelExp)

      // 2. return, for each edge, the set of genes that reliably change along
      //    the edge.
      // TODO print what was refined
      // TODO return per-edge refined clustering
    }
    // 3a. expand cluster-level precedences using the filtered genes for each
    //    edge.
    // 3b. alternative: intersect the genes for every edge of a cluster.
  }

  def refineClusteringForEdgeLabels(
    e: UnlabeledEdge[StateGraphVertex],
    clustering: Map[String, Set[String]],
    geneLevelExp: Experiment[Double]
  ): Map[String, Set[String]] = {
    for (label <- UndirectedStateGraphOps.edgeLabels(e)) {
      // get members for cluster "label"
      val labelMembers = clustering(label)

      // depending on the up/down switch on the label, check if each label
      // member
    }
  }

  def buildDirectedStateGraphsForAllClusterings():
      Seq[(Map[String, Set[String]], DirectedBooleanStateGraph)] = {
    val smoothedExperiment = getSmoothedExperiment()

    val clusterings = HierarchicalClustering.computeHierarchicalClustering(
      smoothedExperiment, opts.clusteringOpts.maxNbClusters).drop(
      opts.clusteringOpts.minNbClusters - 1)

    clusterings map { clustering =>
      (clustering,
        graphAndSourcesFromClusterAverages(smoothedExperiment, clustering)._1)
    }
  }

  private def graphAndSourcesFromClusterAverages(
    nonClusteredExperiment: Experiment[Double],
    clustering: Map[String, Set[String]]
  ): (DirectedBooleanStateGraph, Set[StateGraphVertex]) = {
    val avgExp = HierarchicalClustering.experimentFromClusterAverages(
      nonClusteredExperiment, clustering)

    val threeValExp = Experiments.continuousExperimentToThreeValued(avgExp,
      opts.uncertaintyThreshold)

    val expandedBoolExp = StateGraphs.expandWithBooleanCombinations(
      threeValExp)

    val graphBuilder = new IncrementalStateGraphBuilder(expandedBoolExp,
      clustering, trajectories)

    val g = graphBuilder.buildGraph

    (g, graphBuilder.initialNodes(g))
  }

  def getSmoothedExperiment(): ContinuousExperiment = {
    opts.inputFileOpts.smoothedExperimentFile match {
      case Some(f) => ContinuousExperimentParser.parseAndFilter(f, None)
      case None => buildSmoothedExperiment()
    }
  }

  def buildSmoothedExperiment(): ContinuousExperiment = {
    val normalizedExpAfterFiltering = getNormalizedFilteredExperiment()
    BinomialMLE.run(normalizedExpAfterFiltering, trajectories,
      opts.smoothingRadius)
  }

  def getNormalizedFilteredExperiment(): BooleanExperiment = {
    opts.inputFileOpts.discretizedExperimentFile match {
      case Some(f) => BooleanExperimentParser.parseAndFilter(f, None)
      case None => buildNormalizedFilteredExperiment()
    }
  }

  def buildNormalizedFilteredExperiment(): BooleanExperiment = {
    val continuousExperiment = getTransformedContinuousExperiment()

    val booleanNormalizedExp = Discretization.binarize(continuousExperiment,
      opts.booleanNormalizationMethod)

    // ExperimentHistograms.plotLabeledHistograms(continuousExperiment,
    //   booleanNormalizedExp, reporter.file("boolean-normalization-histograms"))

    val filteredByNbLevels = filterOutNamesWithSingleValue(booleanNormalizedExp)
    filterDifferentialVars(filteredByNbLevels)
  }

  def filterOutNamesWithSingleValue(
    experiment: BooleanExperiment
  ): BooleanExperiment = {
    // TODO this is not a transformation any more, move code
    val namesWithSingleValue =
      ExperimentTransformation.namesWithSingleValue(experiment)
    val namesWithMultipleValues = experiment.names.toSet -- namesWithSingleValue
    experiment.project(namesWithMultipleValues)
  }

  def filterDifferentialVars(experiment: BooleanExperiment): BooleanExperiment = {
    // TODO this is not a transformation any more, move code
    val differentialNames = ExperimentTransformation.differentialNames(
      experiment, opts.minDifferentialThreshold)
    experiment.project(differentialNames)
  }

  def getTransformedContinuousExperiment(): ContinuousExperiment = {
    val file = opts.inputFileOpts.continuousExperimentFile.getOrElse(
      sys.error("no continuous experiment given"))
    val parsedExperiment = ContinuousExperimentParser.parseAndFilter(file,
      geneNamesToFilter)
    transformExperiment(NamingUtil.canonicalizeNames(parsedExperiment))
  }

  def transformExperiment(exp: ContinuousExperiment): ContinuousExperiment = {
    opts.pseudoLogFactor match {
      case Some(factor) => ExperimentTransformation.pseudoLog(exp, factor)
      case None => exp
    }
  }
}
