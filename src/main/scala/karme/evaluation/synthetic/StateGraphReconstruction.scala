package karme.evaluation.synthetic

import karme.Reporter
import karme.graphs.StateGraphs.{DirectedBooleanStateGraph, StateGraphVertex}
import karme.transformations.{IncrementalStateGraphBuilder, MultiHammingEdgeExpansion}

class StateGraphReconstruction(implicit reporter: Reporter) {

  def reconstructStateGraph(
    V: Set[StateGraphVertex],
    nodePartialOrder: PartialOrdering[StateGraphVertex]
  ): DirectedBooleanStateGraph = {
    val multiHammingGraph = new IncrementalStateGraphBuilder(V,
      nodePartialOrder).buildGraph

    new MultiHammingEdgeExpansion(multiHammingGraph).expandMultiHammingEdges()
  }

}
