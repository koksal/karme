package karme.transformations

import karme.CellTrajectories.CellTrajectory
import karme.Experiments.{BooleanExperiment, ContinuousExperiment, ThreeValuedExperiment}
import karme.{Experiments, SynthInputBuilderOpts}
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.{DirectedBooleanStateGraph, UndirectedBooleanStateGraph, UndirectedStateGraphOps}
import karme.parsing.{CellTrajectoryParser, ContinuousExperimentParser}
import karme.transformations.clustering.HierarchicalClustering

class SynthesisInputBuilder(opts: SynthInputBuilderOpts) {

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

    if (opts.clusteringOpts.cluster) {
      val geneClustering = HierarchicalClustering.clusterVariables(
        smoothedExperiment, ???, opts.clusteringOpts.minNbClusters,
        opts.clusteringOpts.maxNbClusters, ???)

      HierarchicalClustering.experimentFromClusterAverages(smoothedExperiment,
        geneClustering, ???)
    } else {
      smoothedExperiment
    }
  }

  def getSmoothedExperiment(): ContinuousExperiment = {
    opts.inputFileOpts.mleExperimentFile match {
      case Some(f) => ContinuousExperimentParser.parseAndFilter(f, None)
      case None => buildSmoothedExperiment()
    }
  }

  def buildSmoothedExperiment(): ContinuousExperiment = {
    // TODO name it Boolean normalization.

  }

}
