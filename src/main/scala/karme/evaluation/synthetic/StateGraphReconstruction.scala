package karme.evaluation.synthetic

import karme.Reporter
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex
import karme.transformations.IncrementalStateGraphBuilder
import karme.transformations.MultiHammingEdgeExpansion

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
