package karme.transformations

import karme.Experiments.Experiment
import karme.graphs.StateGraphs.DirectedBooleanStateGraph

class IncrementalStateGraphBuilder(
  exp: Experiment[Double],
  clustering: Map[String, Set[String]]
) {

  def buildGraph: DirectedBooleanStateGraph = {
    // pre-compute pairwise rank-sum p-values (in another class)
    // using those p-values or averages, choose initial state(s)
    // increase connectivity, one state at a time, by adding a least-Hamming
    // edge

    ???
  }
}
