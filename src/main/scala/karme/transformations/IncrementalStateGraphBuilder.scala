package karme.transformations

import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.DirectedBooleanStateGraph

class IncrementalStateGraphBuilder(
  exp: Experiment[Boolean],
  clustering: Map[String, Set[String]],
  trajectories: Seq[CellTrajectory]
) {

  val V = StateGraphs.nodesFromExperiment(exp)

  val nodePartialOrdering = new NodeComparisonByPseudotimeRankSum(V.toSeq,
    trajectories).partialOrdering

  def buildGraph: DirectedBooleanStateGraph = {
    // using those p-values or averages, choose initial state(s)
    // increase connectivity, one state at a time, by adding a least-Hamming
    // edge

    ???
  }

}
