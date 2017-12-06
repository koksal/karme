package karme.evaluation.synthetic

import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.transformations.DistributionComparisonTest
import karme.transformations.IncrementalStateGraphBuilder
import karme.transformations.MultiHammingEdgeExpansion

object StateGraphReconstruction {

  def reconstructStateGraph(
    experiment: Experiment[Boolean],
    trajectory: CellTrajectory,
    distributionComparisonTest: DistributionComparisonTest,
    distributionComparisonPValue: Double
  ): DirectedBooleanStateGraph = {
    val multiHammingGraph = new IncrementalStateGraphBuilder(
      experiment, Seq(trajectory), distributionComparisonTest,
      distributionComparisonPValue).buildGraph

    new MultiHammingEdgeExpansion(multiHammingGraph).expandMultiHammingEdges()
  }

}
