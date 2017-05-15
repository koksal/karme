package karme.transformations

import karme.AnnotationContext
import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment
import karme.Experiments.{BooleanExperiment, ContinuousExperiment}
import karme.Reporter
import karme.{Experiments, InputTransformerOpts}
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex
import karme.parsing.{BooleanExperimentParser, CellTrajectoryParser, ContinuousExperimentParser, NamesParser}
import karme.transformations.clustering.HierarchicalClustering
import karme.transformations.discretization.Discretization
import karme.transformations.smoothing.BinomialMLE
import karme.util.NamingUtil
import karme.visualization.CurvePlot

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

  def buildStateGraphAndSources:
      (DirectedBooleanStateGraph, Set[StateGraphVertex]) = {
    if (opts.cluster) {
      stateGraphAndSourcesForBestClustering(getSmoothedExperiment())
    } else {
      ???
    }
  }

  private def stateGraphAndSourcesForBestClustering(
    exp: Experiment[Double]
  ): (DirectedBooleanStateGraph, Set[StateGraphVertex]) = {
    // TODO clustering should be a val that can be reaccessed

    val geneClustering = HierarchicalClustering.computeBestClustering(exp,
      opts.clusteringOpts)

    // TODO move this to visualization phase
    new CurvePlot(reporter).plotClusterCurves(exp,
      trajectories, geneClustering, "smoothed-experiment")

    graphAndSourcesFromClusterAverages(exp, geneClustering)
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
