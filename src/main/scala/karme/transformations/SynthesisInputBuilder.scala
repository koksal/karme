package karme.transformations

import karme.AnnotationContext
import karme.CellTrajectories.CellTrajectory
import karme.Experiments.{BooleanExperiment, ContinuousExperiment, ThreeValuedExperiment}
import karme.{Experiments, SynthInputBuilderOpts}
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.{DirectedBooleanStateGraph, UndirectedBooleanStateGraph, UndirectedStateGraphOps}
import karme.parsing.{BooleanExperimentParser, CellTrajectoryParser, ContinuousExperimentParser, NamesParser}
import karme.transformations.clustering.HierarchicalClustering
import karme.transformations.discretization.Discretization
import karme.transformations.smoothing.BinomialMLE

class SynthesisInputBuilder(
  opts: SynthInputBuilderOpts,
  annotationContext: AnnotationContext
) {

  lazy val trajectories: Seq[CellTrajectory] = {
    opts.inputFileOpts.trajectoryFiles map CellTrajectoryParser.parse
  }

  def buildDirectedStateGraph(): DirectedBooleanStateGraph = {
    val undirectedStateGraph = buildUndirectedStateGraph()
    UndirectedStateGraphOps.orientByTrajectories(undirectedStateGraph,
      trajectories)
  }

  def buildUndirectedStateGraph(): UndirectedBooleanStateGraph = {
    val booleanExperiment = buildBooleanExpWithStateGraphStates()
    StateGraphs.fromBooleanExperiment(booleanExperiment,
      opts.maxHammingDistance)
  }

  def buildBooleanExpWithStateGraphStates(): BooleanExperiment = {
    val threeValuedExperiment = buildThreeValuedExperiment()
    StateGraphs.expandWithBooleanCombinations(threeValuedExperiment)
  }

  def buildThreeValuedExperiment(): ThreeValuedExperiment = {
    Experiments.continuousExperimentToThreeValued(
      processContinuousExperimentForDiscretization(),
      opts.uncertaintyThreshold)
  }

  def processContinuousExperimentForDiscretization(): ContinuousExperiment = {
    val smoothedExperiment = getSmoothedExperiment()

    if (opts.cluster) {
      val geneClustering = HierarchicalClustering.clusterVariables(
        smoothedExperiment, annotationContext.annotationVariables,
        opts.clusteringOpts)

      HierarchicalClustering.experimentFromClusterAverages(smoothedExperiment,
        geneClustering, annotationContext.annotationVariables)
    } else {
      smoothedExperiment
    }
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
    val filteredByNbLevels = filterOutNamesWithSingleValue(booleanNormalizedExp)
    filterByActivity(filteredByNbLevels)
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

  def filterByActivity(experiment: BooleanExperiment): BooleanExperiment = {
    // TODO this is not a transformation any more, move code
    val inactiveNames = ExperimentTransformation.inactiveVariables(experiment,
      opts.cellActivityThreshold)
    val activeNames = experiment.names.toSet -- inactiveNames
    experiment.project(activeNames)
  }

  def getTransformedContinuousExperiment(): ContinuousExperiment = {
    val file = opts.inputFileOpts.continuousExperimentFile.getOrElse(
      sys.error("no continuous experiment given"))
    val parsedExperiment = ContinuousExperimentParser.parseAndFilter(file,
      getNamesToFilter())
    transformExperiment(parsedExperiment)
  }

  def getNamesToFilter(): Option[Set[String]] = {
    val parsedFilterNames = NamesParser(opts.inputFileOpts.namesFiles)
    if (parsedFilterNames.isEmpty) {
      None
    } else {
      Some(parsedFilterNames)
    }
  }

  def transformExperiment(exp: ContinuousExperiment): ContinuousExperiment = {
    opts.pseudoLogFactor match {
      case Some(factor) => ExperimentTransformation.pseudoLog(exp, factor)
      case None => exp
    }
  }
}