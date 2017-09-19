package karme.transformations

import karme.CellTrajectories.CellTrajectory
import karme.Experiments.{BooleanExperiment, ContinuousExperiment, Experiment}
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.{DirectedBooleanStateGraph, StateGraphVertex}
import karme.transformations.clustering.GeneClustering
import karme.transformations.discretization.Discretization
import karme.transformations.smoothing.BinomialMLE
import karme.util.NamingUtil
import karme.visualization.{CurvePlot, ExperimentHistogramPlotter, StateGraphPlotter}
import karme._

case class TransformResult(
  graph: DirectedBooleanStateGraph,
  sources: Set[StateGraphVertex],
  clustering: Option[Clustering]
)

class InputTransformer(
  val rawExperiment: Experiment[Double],
  val trajectories: Seq[CellTrajectory],
  val opts: InputTransformerOpts,
  val annotationContext: AnnotationContext
)(implicit reporter: Reporter) {

  val clusteringModule = new GeneClustering(opts.clusteringOpts)

  def transform(): TransformResult = {
    val smoothedExp = buildSmoothedExperiment()

    if (opts.plotSmoothedGeneCurves) {
      new CurvePlot().plotCurvesPerGene(smoothedExp,
        trajectories, reporter.file("smoothed-curves"))
    }

    var clustering: Option[Clustering] = None
    if (opts.clusteringOpts.cluster) {
      clustering = Some(clusteringModule.computeBestClustering(smoothedExp))
    }

    var (graph, sources) = if (opts.clusteringOpts.cluster) {
      val avgExp = clusteringModule.experimentFromClusterAverages(
        smoothedExp, clustering.get.clusterToMembers)
      makeGraphAndSources(avgExp)
    } else {
      makeGraphAndSources(smoothedExp)
    }

    println("Done building graph.")

    if (opts.clusteringOpts.cluster && opts.clusteringOpts.refineClusters) {
      val refiner = new ClusteringRefiner(graph, smoothedExp, clustering.get,
        DistributionComparisonTest.fromOptions(
          opts.distributionComparisonMethod),
        opts.clusteringOpts.clusterRefinementPValue)
      val perEdgeRft = refiner.refineClusteringPerEdge()
      clustering =
        Some(Clustering.combineByIntersection(perEdgeRft.values.toSeq))
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
        clustering.get.clusterToMembers, "smoothed-experiment")
    }

    TransformResult(graph, sources, clustering)
  }

  def buildDirectedStateGraphsForAllClusterings():
      Seq[(Map[String, Set[String]], DirectedBooleanStateGraph)] = {
    val smoothedExperiment = buildSmoothedExperiment()

    val clusterings = clusteringModule.computeHierarchicalClustering(
      smoothedExperiment, opts.clusteringOpts.maxNbClusters).drop(
      opts.clusteringOpts.minNbClusters - 1)

    clusterings map { clustering =>
      val avgExp = clusteringModule.experimentFromClusterAverages(
        smoothedExperiment, clustering)
      (clustering, makeGraphAndSources(avgExp)._1)
    }
  }

  def makeGraphAndSources(
    exp: Experiment[Double]
  ): (DirectedBooleanStateGraph, Set[StateGraphVertex]) = {
    println("Building three-valued experiment.")

    val threeValExp = Experiments.continuousExperimentToThreeValued(exp,
      opts.uncertaintyThreshold)

    println("Done building three-valued experiment.")

    if (opts.plotThreeValuedData) {
      new ExperimentHistogramPlotter().plotLabeledHistograms(exp, threeValExp,
        reporter.file("three-valued-data"))
    }

    val boolExp = StateGraphs.removeMeasurementsWithUncertainDiscretization(
      threeValExp)

    val graphBuilder = new IncrementalStateGraphBuilder(boolExp, trajectories,
      DistributionComparisonTest.fromOptions(opts.distributionComparisonMethod))

    val g = graphBuilder.buildGraph

    (g, graphBuilder.initialNodes(g))
  }

  def buildSmoothedExperiment(): ContinuousExperiment = {
    val normalizedExpAfterFiltering = buildNormalizedFilteredExperiment()

    if (opts.plotBinarizedGeneCurves) {
      new CurvePlot().plotBooleanCurvesPerGene(normalizedExpAfterFiltering,
        trajectories, reporter.file("non-smoothed-curves"))
    }

    println("Running smoothing.")
    BinomialMLE.run(normalizedExpAfterFiltering, trajectories,
      opts.smoothingRadius)
  }

  def buildNormalizedFilteredExperiment(): BooleanExperiment = {
    val continuousExperiment = getTransformedContinuousExperiment()

    val booleanNormalizedExp = Discretization.binarize(continuousExperiment,
      opts.booleanNormalizationMethod)

    if (opts.plotBinarizedData) {
      new ExperimentHistogramPlotter().plotLabeledHistograms(continuousExperiment,
        booleanNormalizedExp, reporter.file("binarized-data"))
    }

    new DifferentialGeneFiltering(opts.minDifferentialThreshold)
      .filterSymmetric(booleanNormalizedExp)
  }

  def getTransformedContinuousExperiment(): ContinuousExperiment = {
    val filteredCellExperiment = filterCellsByTrajectories(rawExperiment)
    val transformedExperiment = transformExperiment(
      NamingUtil.canonicalizeNames(filteredCellExperiment))

    if (opts.plotOriginalData) {
      new ExperimentHistogramPlotter().plotHistogramsPerVariable(filteredCellExperiment,
        reporter.file("original-data"))
    }

    if (opts.plotTransformedData) {
      new ExperimentHistogramPlotter().plotHistogramsPerVariable(transformedExperiment,
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
