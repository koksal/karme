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
import karme.transformations.ExpressionDerivation.Unchanged
import karme.transformations.clustering.DerivativeClustering
import karme.transformations.clustering.GeneClustering
import karme.transformations.discretization.Discretization
import karme.transformations.smoothing.BinomialMLE
import karme.util.FileUtil
import karme.util.NamingUtil
import karme.visualization.CurvePlot
import karme.visualization.HistogramPlotter
import karme.visualization.StateGraphPlotter

case class TransformResult(
  graph: DirectedBooleanStateGraph,
  sources: Set[StateGraphVertex],
  clustering: Clustering,
  perEdgeClustering: Map[UnlabeledEdge[StateGraphVertex], Clustering]
)

class InputTransformer(
  val opts: InputTransformerOpts,
  val annotationContext: AnnotationContext
)(implicit reporter: Reporter) {

  val trajectories: Seq[CellTrajectory] = {
    opts.inputFileOpts.trajectoryFiles map CellTrajectoryParser.parse
  }

  val geneNamesToFilter: Option[Set[String]] = {
    NamesParser.parseNameUnion(opts.inputFileOpts.namesFiles)
  }

  lazy val inputExperiment: Experiment[Double] = {
    val file = opts.inputFileOpts.continuousExperimentFile.getOrElse(
      sys.error("No continuous experiment given."))
    ContinuousExperimentParser.parseAndFilter(file, geneNamesToFilter)
  }

  val clusteringModule = new GeneClustering(opts.clusteringOpts)

  def transform(): TransformResult = {
    val smoothedExp = getSmoothedExperiment()

    if (opts.plotSmoothedGeneCurves) {
      new CurvePlot().plotCurvesPerGene(smoothedExp,
        trajectories, reporter.file("smoothed-curves"))
    }

    var clustering = clusteringModule.computeBestClustering(smoothedExp)
    var (graph, sources) = graphAndSourcesFromClusterAverages(smoothedExp,
      clustering)

    if (opts.clusteringOpts.refineClusters) {
      val refiner = new ClusteringRefiner(graph, smoothedExp, clustering,
        DistributionComparisonTest.fromOptions(
          opts.distributionComparisonMethod),
        opts.clusteringOpts.clusterRefinementPValue)
      val perEdgeRft = refiner.refineClusteringPerEdge()
      clustering = Clustering.combineByIntersection(perEdgeRft.values.toSeq)
    }

    val plotter = new StateGraphPlotter(reporter)
    plotter.plotDirectedGraph(graph, "graph-before-expansion",
      cellClustering = annotationContext.cellClustering,
      nodeHighlightGroups = List(sources.map(_.state)))

    graph = new MultiHammingEdgeExpansion(graph).expandMultiHammingEdges()
    plotter.plotDirectedGraph(graph, "graph-after-expansion",
      cellClustering = annotationContext.cellClustering,
      nodeHighlightGroups = List(sources.map(_.state)))

    if (opts.plotClusterCurves) {
      new CurvePlot().plotClusterCurves(smoothedExp, trajectories,
        clustering.clusterToMembers, "smoothed-experiment")
    }

    val emptyRefinement = Map[UnlabeledEdge[StateGraphVertex], Clustering]()
    TransformResult(graph, sources, clustering, emptyRefinement)
  }

  def buildDirectedStateGraphsForAllClusterings():
      Seq[(Map[String, Set[String]], DirectedBooleanStateGraph)] = {
    val smoothedExperiment = getSmoothedExperiment()

    val clusterings = clusteringModule.computeHierarchicalClustering(
      smoothedExperiment, opts.clusteringOpts.maxNbClusters).drop(
      opts.clusteringOpts.minNbClusters - 1)

    clusterings map { clustering =>
      (clustering, graphAndSourcesFromClusterAverages(
        smoothedExperiment, Clustering(clustering))._1)
    }
  }

  def graphAndSourcesFromClusterAverages(
    nonClusteredExperiment: Experiment[Double],
    clustering: Clustering
  ): (DirectedBooleanStateGraph, Set[StateGraphVertex]) = {
    val avgExp = clusteringModule.experimentFromClusterAverages(
      nonClusteredExperiment, clustering.clusterToMembers)

    val threeValExp = Experiments.continuousExperimentToThreeValued(avgExp,
      opts.uncertaintyThreshold)

    if (opts.plotThreeValuedClusterData) {
      new HistogramPlotter().plotLabeledHistograms(avgExp, threeValExp,
        reporter.file("three-valued-cluster-data"))
    }

    val boolExp = StateGraphs.removeStatesWithUncertainValues(threeValExp)

    if (opts.plotBinarizedClusterData) {
      new HistogramPlotter().plotLabeledHistograms(avgExp, boolExp,
        reporter.file("binarized-cluster-data"))
    }

    val graphBuilder = new IncrementalStateGraphBuilder(boolExp,
      clustering.clusterToMembers, trajectories,
      DistributionComparisonTest.fromOptions(opts.distributionComparisonMethod))

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

    if (opts.plotBinarizedGeneCurves) {
      new CurvePlot().plotBooleanCurvesPerGene(normalizedExpAfterFiltering,
        trajectories, reporter.file("non-smoothed-curves"))
    }

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
      new HistogramPlotter().plotLabeledHistograms(continuousExperiment,
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
    val filteredCellExperiment = filterCellsByTrajectories(parsedExperiment)
    val transformedExperiment = transformExperiment(
      NamingUtil.canonicalizeNames(filteredCellExperiment))

    if (opts.plotOriginalData) {
      new HistogramPlotter().plotHistogramsPerVariable(filteredCellExperiment,
        reporter.file("original-data"))
    }

    if (opts.plotTransformedData) {
      new HistogramPlotter().plotHistogramsPerVariable(transformedExperiment,
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

  def filterCellsByTrajectories(exp: Experiment[Double]): Experiment[Double] = {
    val allTrajectoryMeasurementIDs = trajectories.flatMap(_.keySet).toSet
    val trajectoryMeasurements = exp.measurements filter {
      ms => allTrajectoryMeasurementIDs.contains(ms.id)
    }
    exp.copy(measurements = trajectoryMeasurements)
  }
}
