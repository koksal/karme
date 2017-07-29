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
import karme.transformations.clustering.GeneClustering
import karme.transformations.discretization.Discretization
import karme.transformations.smoothing.BinomialMLE
import karme.util.NamingUtil
import karme.visualization.CurvePlot
import karme.visualization.HistogramPlotter

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

  val clusteringModule = new GeneClustering(opts.clusteringOpts)

  def transform(): TransformResult = {
    val smoothedExp = getSmoothedExperiment()

    if (opts.plotSmoothedGeneCurves) {
      new CurvePlot().plotCurvesPerGene(smoothedExp,
        trajectories, reporter.file("smoothed-curves"))
    }

    val nonRefinedClustering = clusteringModule.computeBestClustering(
      smoothedExp)

    val (graph, sources) = graphAndSourcesFromClusterAverages(smoothedExp,
      nonRefinedClustering)

    val clusteringRefiner = new ClusteringRefiner(graph, smoothedExp,
      Clustering(nonRefinedClustering), opts.clusterRefinementPValue)

    val (geneClustering, edgeToRefinedClustering) = if (opts.refineClusters) {
      val edgeToRefinedClustering = clusteringRefiner.refineClusteringPerEdge()

      val geneClustering = Clustering.combineByIntersection(
        edgeToRefinedClustering.values.toSeq)
      (geneClustering, edgeToRefinedClustering)
    } else {
      val geneClustering = Clustering(nonRefinedClustering)
      val emptyRefinement = Map[UnlabeledEdge[StateGraphVertex], Clustering]()
      (geneClustering, emptyRefinement)
    }

    if (opts.plotClusterCurves) {
      new CurvePlot().plotClusterCurves(smoothedExp, trajectories,
        nonRefinedClustering, "smoothed-experiment")
    }

    TransformResult(graph, sources, geneClustering, edgeToRefinedClustering)
  }

  def buildDirectedStateGraphsForAllClusterings():
      Seq[(Map[String, Set[String]], DirectedBooleanStateGraph)] = {
    val smoothedExperiment = getSmoothedExperiment()

    val clusterings = clusteringModule.computeHierarchicalClustering(
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
    val avgExp = clusteringModule.experimentFromClusterAverages(
      nonClusteredExperiment, clustering)

    val threeValExp = Experiments.continuousExperimentToThreeValued(avgExp,
      opts.uncertaintyThreshold)

    val expandedBoolExp = StateGraphs.expandWithBooleanCombinations(
      threeValExp)

    if (opts.plotBinarizedClusterData) {
      new HistogramPlotter().plotLabeledHistograms(avgExp, expandedBoolExp,
        reporter.file("binarized-cluster-data"))
    }

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
