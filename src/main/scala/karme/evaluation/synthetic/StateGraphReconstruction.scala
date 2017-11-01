package karme.evaluation.synthetic

import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.transformations.AverageComparisonTest
import karme.transformations.IncrementalStateGraphBuilder
import karme.transformations.MultiHammingEdgeExpansion

object StateGraphReconstruction {

  def reconstructStateGraph(
    simulationStateToTimestamps: Set[(ConcreteBooleanState, Seq[Int])]
  ): DirectedBooleanStateGraph = {
    val (exp, traj) = SimulationToExperiment.makeExperimentAndTrajectory(
      simulationStateToTimestamps)

    val multiHammingGraph = new IncrementalStateGraphBuilder(exp, Seq(traj),
      new AverageComparisonTest).buildGraph

    new MultiHammingEdgeExpansion(multiHammingGraph).expandMultiHammingEdges()
  }

}
