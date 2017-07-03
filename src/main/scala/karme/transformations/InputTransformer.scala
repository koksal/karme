package karme.transformations

import karme.AnnotationContext
import karme.CellTrajectories.CellTrajectory
import karme.Clustering
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
import karme.visualization.ExperimentHistograms

case class TransformResult(
  graph: DirectedBooleanStateGraph,
  sources: Set[StateGraphVertex],
  clustering: Clustering,
  perEdgeClustering: Map[UnlabeledEdge[StateGraphVertex], Clustering]
)

class InputTransformer(
  opts: InputTransformerOpts,
  annotationContext: AnnotationContext
)(implicit reporter: Reporter) {

  val trajectories: Seq[CellTrajectory] = {
    opts.inputFileOpts.trajectoryFiles map CellTrajectoryParser.parse
  }

  val geneNamesToFilter: Option[Set[String]] = {
    new NamesParser(opts.inputFileOpts.namesFiles).names
  }

  lazy val inputExperiment: Experiment[Double] = {
    val file = opts.inputFileOpts.continuousExperimentFile.getOrElse(
      sys.error("No continuous experiment given."))
    ContinuousExperimentParser.parseAndFilter(file, geneNamesToFilter)
  }

  def transform(): TransformResult = {
    val smoothedExp = getSmoothedExperiment()

    val nonRefinedClustering = HierarchicalClustering.computeBestClustering(
      smoothedExp, opts.clusteringOpts)

    new CurvePlot(reporter).plotClusterCurves(smoothedExp, trajectories,
      nonRefinedClustering, "smoothed-experiment")

    val (graph, sources) = graphAndSourcesFromClusterAverages(smoothedExp,
      nonRefinedClustering)

    val clusteringRefiner = new ClusteringRefiner(graph, smoothedExp,
      Clustering(nonRefinedClustering), opts.clusterRefinementPValue)
    val edgeToRefinedClustering = clusteringRefiner.refineClusteringPerEdge()

    val geneClustering = if (opts.refineClusters) {
      Clustering.combineByIntersection(edgeToRefinedClustering.values.toSeq)
    } else {
      Clustering(nonRefinedClustering)
    }

    TransformResult(graph, sources, geneClustering, edgeToRefinedClustering)
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

    if (opts.plotBinarizedData) {
      ExperimentHistograms.plotLabeledHistograms(continuousExperiment,
        booleanNormalizedExp, reporter.file("binarized-data"))
    }

    new DifferentialGeneFiltering(opts.minDifferentialThreshold)
      .filterSymmetric(booleanNormalizedExp)
  }

  def getTransformedContinuousExperiment(): ContinuousExperiment = {
    val file = opts.inputFileOpts.continuousExperimentFile.getOrElse(
      sys.error("no continuous experiment given"))
    val parsedExperiment = ContinuousExperimentParser.parseAndFilter(file,
      geneNamesToFilter)
    val transformedExperiment = transformExperiment(
      NamingUtil.canonicalizeNames(parsedExperiment))

    if (opts.plotOriginalData) {
      ExperimentHistograms.plotHistogramsPerVariable(parsedExperiment,
        reporter.file("original-data"))
    }

    if (opts.plotTransformedData) {
      ExperimentHistograms.plotHistogramsPerVariable(transformedExperiment,
        reporter.file("transformed-data"))
    }

    transformedExperiment
  }

  def transformExperiment(exp: ContinuousExperiment): ContinuousExperiment = {
    opts.pseudoLogFactor match {
      case Some(factor) => ExperimentTransformation.pseudoLog(exp, factor)
      case None => exp
    }
  }
}
